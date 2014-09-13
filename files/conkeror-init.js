// -*- Mode: js -*-

//allow for 'contrib' stuff
load_paths.unshift("chrome://conkeror-contrib/content/");
// Mode-line
mode_line_mode(true);

require("session");
require("duckduckgo");
// require("google-search-results");
require("gmail");
require("github");
require("feedly");
require("clicks-in-new-buffer");
require("adblockplus");
require("favicon");
// require("confluence");
// require("dom-inspector");

add_hook("mode_line_hook", mode_line_adder(buffer_icon_widget), true);
read_buffer_show_icons = true;
// we'd like to see the # of buffers being loaded 
add_hook("mode_line_hook", mode_line_adder(loading_count_widget), true);
// but really we'd also like to know how many buffers are present and which is the current
add_hook("mode_line_hook", mode_line_adder(buffer_count_widget), true);
// remove the clock
remove_hook("mode_line_hook", mode_line_adder(clock_widget));

url_completion_use_bookmarks = false;
url_completion_use_history = true;
// display the url before going to it in hints mode
hints_display_url_panel = true;
url_remoting_fn = load_url_in_new_buffer;
session_auto_save_auto_load = true;
clicks_in_new_buffer_target = OPEN_NEW_BUFFER_BACKGROUND;
download_buffer_automatic_open_target = OPEN_NEW_BUFFER_BACKGROUND;

// default directory for downloads and shell commands.
cwd = get_home_directory();
cwd.append("Downloads");

// google_search_bind_number_shortcuts();

// make C-c C-c "submit form"
define_key(content_buffer_normal_keymap, "C-c C-c", "submit-form");

// editor_shell_command = "/home/bakhti/bin/emacsclient --eval '(ab/make-formfiller-frame)'";

// user_pref("spellchecker.dictionary","en_US);
