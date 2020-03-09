-- Modified from http://pavelmakhov.com/awesome-wm-widgets/#tabMpdarc_Widget

local awful = require("awful")
local beautiful = require("beautiful")
local spawn = require("awful.spawn")
local wibox = require("wibox")
local gears = require("gears")

local GET_MPD_CMD = [[bash -c "mpc -f '%artist%;%title%;%album%';~/rust_prac/get_cover_path/target/release/get_cover_path"]]
local TOGGLE_MPD_CMD = "mpc toggle"
local PAUSE_MPD_CMD = "mpc pause"
local STOP_MPD_CMD = "mpc stop"
local NEXT_MPD_CMD = "mpc next"
local PREV_MPD_CMD = "mpc prev"

local function split(string_to_split, separator)
    if separator == nil then separator = "%s" end
    local t={}

    for str in string.gmatch(string_to_split, "([^".. separator .."]+)") do
        table.insert(t, str)
    end

    return t
end


local text_icon = wibox.widget {
    widget = wibox.widget.textbox,
    font = 'FuraCode Nerd Font Mono 9',
    markup = "&#xf909;",
    align = 'center',
    valign = 'center',
}

local mpd = wibox.widget {
    text_icon,
    max_value = 1,
    value = 0.75,
    thickness = 2,
    start_angle = 4.71238898, -- 2pi*3/4
    forced_height = 32,
    forced_width = 32,
    rounded_edge = true,
    paddings = 0,
    widget = wibox.container.arcchart
}

local popup = awful.popup {
    ontop = true,
    visible = false,
    shape = gears.shape.rectangle,
    border_width = 5,
    border_color = "#282828",
    bg = "#1d2021",
    fg = "#ebdbb2",
    maximum_height = 180,
    maximum_width = 800,
    widget = {}
}

local current_song_widget = wibox.widget {
    id = 'current_song',
    widget = wibox.widget.textbox,
    font = 'WenQuanYi Micro Hei 9'
}

local controls = wibox.widget {
    spacing = 10,
    layout = wibox.layout.fixed.horizontal,
    {
        id = 'prev',
        markup = "&#xf9ad;",
        font = 'FuraCode Nerd Font Mono',
        widget = wibox.widget.textbox,
    },
    {
        id = 'stop',
        markup = "&#xf9da;",
        font = 'FuraCode Nerd Font Mono',
        widget = wibox.widget.textbox,
    },
    {
        id = 'toggle',
        font = 'FuraCode Nerd Font Mono',
        widget = wibox.widget.textbox,
    },
    {
        id = 'next',
        markup = "&#xf9ac;",
        font = 'FuraCode Nerd Font Mono',
        widget = wibox.widget.textbox,
    },
}

local img = wibox.widget {
    widget = wibox.widget.imagebox,
    align = 'center',
    valign = 'center',
    forced_width = 160,
}

local function stat_update(widget, stdout)
    local current_song = string.gmatch(stdout, "[^\r\n]+")()
    local t = split(current_song, ";")
    local artist = t[1]
    local name = t[2]
    local album = t[3]
    local path = string.match(stdout, "(/home/[%S% ]*)")
    local p, s = string.match(stdout, "#(%d+)/(%d+)%s")
    img:set_image(path)
    stdout = string.gsub(stdout, "\n", "")
    local mpd_percent = string.match(stdout, "(%d?%d)%%")
    local time = string.match(stdout, "(%d:%d+/%d:%d+)%s%(")
    local mpdstatus = string.match(stdout, "%[(%a+)%]")
    local song_info = wibox.widget {
        {
            widget = wibox.widget.textbox,
            text = "Song: ".. name,
            font = 'WenQuanYi Micro Hei Mono 9'
        },
        {
            widget = wibox.widget.textbox,
            text = "Artist: ".. artist,
            font = 'WenQuanYi Micro Hei Mono 9'
        },
        {
            widget = wibox.widget.textbox,
            text = "Album: ".. album,
            font = 'WenQuanYi Micro Hei Mono 9'
        },
        {
            {
                id = 'progress_bar',
                widget = wibox.widget.progressbar,
                max_value = 100,
                value = tonumber(mpd_percent),
                forced_height = 16,
                forced_width = 200,
                paddings = 1,
                margins = 4,
                background_color = "#282828",
                shape = gears.shape.rounded_bar,
            },
            {
                widget = wibox.widget.textbox,
                text = " ",
            },
            {
                widget = wibox.widget.textbox,
                text = time,
                font = 'WenQuanYi Micro Hei Mono 9'
            },
            layout = wibox.layout.align.horizontal,
        },
        {
            {
                widget = wibox.widget.progressbar,
                max_value = tonumber(s),
                value = tonumber(p),
                forced_height = 16,
                forced_width = 200,
                paddings = 1,
                margins = 4,
                background_color = "#282828",
                color = "#b16286",
                shape = gears.shape.rounded_bar,
            },
            {
                widget = wibox.widget.textbox,
                text = " ",
            },
            {
                widget = wibox.widget.textbox,
                text = p..'/'..s,
                font = 'WenQuanYi Micro Hei Mono 9'
            },
            layout = wibox.layout.align.horizontal,
        },
        {
            layout = wibox.layout.flex.horizontal,
            {
                widget = wibox.widget.textbox,
                text = " ",
            },
            controls,
            {
                widget = wibox.widget.textbox,
                text = " ",
            },
        },
        spacing = 4,
        layout = wibox.layout.fixed.vertical,
    }
    if mpdstatus == "playing" then
        text_icon.markup = "&#xf8e3;"
        widget.colors = { "#458588" }
        widget.value = tonumber((100-mpd_percent)/100)
        current_song_widget.markup = name
        song_info:get_children_by_id('progress_bar')[1].color = "#458588"
        controls:get_children_by_id('toggle')[1].markup = "&#xf8e3;"
    elseif mpdstatus == "paused" then
        text_icon.markup = "&#xf909;"
        widget.colors = { "#d79921" }
        widget.value = tonumber(mpd_percent/100)
        current_song_widget.markup = name
        song_info:get_children_by_id('progress_bar')[1].color = "#d79921"
        controls:get_children_by_id('toggle')[1].markup = "&#xf909;"
    else
        text_icon.markup = "&#xf9da;"
        if string.len(stdout) == 0 then -- MPD is not running
        current_song_widget.markup = "MPD is not running"
        else
        widget.colors = { "#cc241d" }
        current_song_widget.markup = "stopped."
        end
    end

    popup:setup {
        {
            img,
            {
                orientation = 'vertical',
                forced_width = 15,
                color = beautiful.bg_focus,
                widget = wibox.widget.separator
            },
            song_info,
            layout = wibox.layout.fixed.horizontal,
        },
        margins = 8,
        widget = wibox.container.margin
    }
end

-- watch(GET_MPD_CMD, 1, stat_update, mpd)

gears.timer {
    timeout = 1,
    call_now  = true,
    autostart = true,
    callback = function()
        awful.spawn.easy_async(GET_MPD_CMD, function (stdout)
            stat_update(mpd, stdout)
        end)
    end,
}

mpd:connect_signal("button::press", function(_, _, _, button)
    if (button == 1)        then awful.spawn(TOGGLE_MPD_CMD, false)      -- left click
    elseif (button == 2)    then awful.spawn(STOP_MPD_CMD, false)
    elseif (button == 3)    then awful.spawn(PAUSE_MPD_CMD, false)
    elseif (button == 4)    then awful.spawn(NEXT_MPD_CMD, false)  -- scroll up
    elseif (button == 5)    then awful.spawn(PREV_MPD_CMD, false)  -- scroll down
    end
    spawn.easy_async(GET_MPD_CMD, function(stdout)
        stat_update(mpd, stdout)
    end)
end)

current_song_widget:buttons(
    awful.util.table.join(
        awful.button({}, 1, function()
            if popup.visible then
                popup.visible = not popup.visible
            else
                popup:move_next_to(mouse.current_widget_geometry)
            end
        end)
    )
)

controls:get_children_by_id('toggle')[1]:connect_signal("button::press", function ()
    awful.spawn(TOGGLE_MPD_CMD, false)
    spawn.easy_async(GET_MPD_CMD, function(stdout)
        stat_update(mpd, stdout)
    end)
end)
controls:get_children_by_id('stop')[1]:connect_signal("button::press", function ()
    awful.spawn(STOP_MPD_CMD, false)
    spawn.easy_async(GET_MPD_CMD, function(stdout)
        stat_update(mpd, stdout)
    end)
end)
controls:get_children_by_id('prev')[1]:connect_signal("button::press", function ()
    awful.spawn(PREV_MPD_CMD, false)
    spawn.easy_async(GET_MPD_CMD, function(stdout)
        stat_update(mpd, stdout)
    end)
end)
controls:get_children_by_id('next')[1]:connect_signal("button::press", function ()
    awful.spawn(NEXT_MPD_CMD, false)
    spawn.easy_async(GET_MPD_CMD, function(stdout)
        stat_update(mpd, stdout)
    end)
end)

local mpd_widget = wibox.widget{
    mpd,
    current_song_widget,
    layout = wibox.layout.align.horizontal,
    }
return mpd_widget
