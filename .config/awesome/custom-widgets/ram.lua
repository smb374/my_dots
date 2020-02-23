local awful = require('awful')
local watch = require('awful.widget.watch')
local wibox = require('wibox')
local beautiful = require('beautiful')
local gears = require('gears')

local widget = {}

local function worker()
    local ram_text_widget = wibox.widget {
        widget = wibox.widget.textbox,
        align = 'center',
        valign = 'center'
    }
    local popup = awful.popup {
        ontop = true,
        visible = false,
        shape = gears.shape.rectangle,
        border_width = 5,
        border_color = "#282828",
        maximum_width = 300,
        widget = {}
    }
    local ram_rows = {
        spacing = 4,
        layout = wibox.layout.fixed.vertical,
    }
    -- ram_text_widget:buttons(
    --     awful.util.table.join(
    --         awful.button({}, 1, function()
    --             if popup.visible then
    --                 popup.visible = not popup.visible
    --             else
    --                 popup:move_next_to(mouse.current_widget_geometry)
    --             end
    --         end)
    --     )
    -- )
    ram_text_widget:connect_signal('mouse::enter', function()
        popup:move_next_to(mouse.current_widget_geometry)
    end)
    ram_text_widget:connect_signal('mouse::leave', function ()
        popup.visible = false
    end)
    watch([[bash -c "free | grep -z Mem.*Swap.*"]], 5, function(widget, stdout)
        local total, used, free, buff_cache, total_swap, used_swap, free_swap =
            stdout:match('(%d+)%s*(%d+)%s*(%d+)%s*%d+%s*(%d+)%s*%d+%s*Swap:%s*(%d+)%s*(%d+)%s*(%d+)')
        local function getPercentage(value)
            return math.floor(value / (total+total_swap) * 100 + 0.5)
        end
        local used_ram = getPercentage(used + used_swap)
        widget.markup = "&#xfb19; ".. string.format("%3d", math.floor(used_ram)) .."% "
        local free_ram = getPercentage(free + free_swap)
        local buff_ram = getPercentage(buff_cache)
        local u_row = wibox.widget {
            {
                text = "Used: ",
                forced_width = 80,
                widget = wibox.widget.textbox
            },
            {
                text = string.format("%3d%%", math.floor(used_ram)),
                forced_width = 40,
                widget = wibox.widget.textbox
            },
            {
                max_value = 100,
                value = used_ram,
                forced_height = 20,
                forced_width = 150,
                paddings = 1,
                margins = 4,
                background_color = "#282828",
                color = "#cc241d",
                widget = wibox.widget.progressbar,

            },
            layout = wibox.layout.align.horizontal
        }
        local f_row = wibox.widget {
            {
                text = "Free: ",
                forced_width = 80,
                widget = wibox.widget.textbox
            },
            {
                text = string.format("%3d%%", math.floor(free_ram)),
                forced_width = 40,
                widget = wibox.widget.textbox
            },
            {
                max_value = 100,
                value = free_ram,
                forced_height = 20,
                forced_width = 150,
                paddings = 1,
                margins = 4,
                background_color = "#282828",
                color = "#689d6a",
                widget = wibox.widget.progressbar,

            },
            layout = wibox.layout.align.horizontal
        }
        local b_row = wibox.widget {
            {
                text = "Buffered: ",
                forced_width = 80,
                widget = wibox.widget.textbox
            },
            {
                text = string.format("%3d%%", math.floor(buff_ram)),
                forced_width = 40,
                widget = wibox.widget.textbox
            },
            {
                max_value = 100,
                value = buff_ram,
                forced_height = 20,
                forced_width = 150,
                paddings = 1,
                margins = 4,
                background_color = "#282828",
                color = "#458588",
                widget = wibox.widget.progressbar,

            },
            layout = wibox.layout.align.horizontal
        }
        ram_rows[1] = u_row
        ram_rows[2] = f_row
        ram_rows[3] = b_row
        popup:setup {
            {
                ram_rows,
                layout = wibox.layout.fixed.vertical,
            },
            margins = 8,
            widget = wibox.container.margin
        }
    end, ram_text_widget)
return ram_text_widget
end
return setmetatable(widget, { __call = function()
    return worker()
end })
