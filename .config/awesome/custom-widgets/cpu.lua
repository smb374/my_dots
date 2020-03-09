-- Modified from:
-- https://github.com/streetturtle/awesome-wm-widgets/tree/master/cpu-widget

local awful = require('awful')
local watch = require('awful.widget.watch')
local wibox = require('wibox')
local beautiful = require("beautiful")
local gears = require("gears")

local widget = {}

local function worker()

    local cpu_text_widget = wibox.widget {
        widget = wibox.widget.textbox,
        align = 'center',
        valign = 'center',
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

    local cpu_rows = {
        spacing = 4,
        layout = wibox.layout.fixed.vertical,
    }

    -- cpu_text_widget:buttons(
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
    cpu_text_widget:connect_signal('mouse::enter', function()
        popup:move_next_to(mouse.current_widget_geometry)
    end)
    cpu_text_widget:connect_signal('mouse::leave', function ()
        popup.visible = false
    end)
    local cpus = {}
    watch([[bash -c "cat /proc/stat | grep '^cpu.'"]], 1,
    function (widget, stdout)
        local i = 1
        for line in stdout:gmatch("[^\r\n]+") do
            if cpus[i] == nil then cpus[i] = {} end
            local name, user, nice, system, idle, iowait, irq, softirq, steal =
                line:match('(%w+)%s+(%d+)%s(%d+)%s(%d+)%s(%d+)%s(%d+)%s(%d+)%s(%d+)%s(%d+)%s%d+%s%d+')
            local total = user + nice + system + idle + iowait + irq + softirq + steal
            local diff_idle = idle - tonumber(cpus[i]['idle_prev'] == nil and 0 or cpus[i]['idle_prev'])
            local diff_total = total - tonumber(cpus[i]['total_prev'] == nil and 0 or cpus[i]['total_prev'])
            local diff_usage = (1000 * (diff_total - diff_idle) / diff_total + 5) / 10
            cpus[i]['total_prev'] = total
            cpus[i]['idle_prev'] = idle
            if i == 1 then
                widget.markup = "&#xf85a; ".. string.format("%3d", math.floor(diff_usage)) .."% "
            end
            local row = wibox.widget
            {
                {
                    text = name,
                    forced_width = 40,
                    widget = wibox.widget.textbox
                },
                {
                    text = string.format("%3d%%", math.floor(diff_usage)),
                    forced_width = 40,
                    widget = wibox.widget.textbox
                },
                {
                    max_value = 100,
                    value = diff_usage,
                    forced_height = 10,
                    forced_width = 150,
                    paddings = 1,
                    margins = 4,
                    background_color = "#282828",
                    shape = gears.shape.rounded_bar,
                    color = "#d65d0e",
                    widget = wibox.widget.progressbar,

                },
                layout = wibox.layout.align.horizontal
            }

            cpu_rows[i] = row
            i = i + 1
        end
        popup:setup {
            {
                cpu_rows,
                layout = wibox.layout.fixed.vertical,
            },
            margins = 8,
            widget = wibox.container.margin
        }
    end,
    cpu_text_widget)
return cpu_text_widget
end
return setmetatable(widget, { __call = function()
    return worker()
end })
