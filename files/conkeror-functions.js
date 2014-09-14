// -*- Mode: js -*-

// -----=====-----
function org_store_link (url, title, window) {
    var cmd_str = 'emacsclient \"org-protocol://store-link://'+url+'/'+title+'\"';
    if (window != null) {
        window.minibuffer.message('Issuing ' + cmd_str);
    }
    shell_command_blind(cmd_str);
}

interactive("org-store-link", "Stores [[url][title]] as org link and copies url to emacs kill ring",
            function (I) {
                org_store_link(encodeURIComponent(I.buffer.display_uri_string), encodeURIComponent(I.buffer.document.title), I.window);
            });

function org_capture (url, title, selection, window) {
    var cmd_str = 'emacsclient \"org-protocol://capture://'+url+'/'+title+'/'+selection+'\"';
    if (window != null) {
        window.minibuffer.message('Issuing ' + cmd_str);
    }
    shell_command_blind(cmd_str);
}

interactive("org-capture", "Clip url, title, and selection to capture via org-protocol",
            function (I) {
                org_capture(encodeURIComponent(I.buffer.display_uri_string), encodeURIComponent(I.buffer.document.title), encodeURIComponent(I.buffer.top_frame.getSelection()), I.window);
            });

define_key(content_buffer_normal_keymap, "C-c r", "org-capture");
define_key(content_buffer_normal_keymap, "C-c l", "org-store-link");

//-----=====-----
define_browser_object_class(
    "tinyurl", "Get a tinyurl for the current page",
    function (I, prompt) {
        check_buffer(I.buffer, content_buffer);
        let createurl = 'http://tinyurl.com/api-create.php?url=' +
            encodeURIComponent(
                load_spec_uri_string(
                    load_spec(I.buffer.top_frame)));
        try {
            var content = yield send_http_request(
                load_spec({uri: createurl}));
            yield co_return(content.responseText);
        } catch (e) { }
    });

define_key(content_buffer_normal_keymap, "* q", "browser-object-tinyurl");

interactive("find-clipboard-url", "Open URL from X clipboard",
	   "find-url",
	   $browser_object=
	   function(I) {
	       return read_from_x_primary_selection();
	   }
);
define_key(content_buffer_normal_keymap, "M-t", "find-clipboard-url");

//-----=====-----
// use M-y to google current selection in new buffer
// use M-Y to google current selection in new buffer "double-quoted"

// [ref: http://www.mozdev.org/pipermail/conkeror/2009-February/001334.html ]
// (See also "**c" for selecting text)
interactive("search-clipboard-contents", "Search in Google the content of the X clipboard (the selected text)",
              "find-url",
              $browser_object=
              function(I) {
                  return "duckduckgo "+ read_from_x_primary_selection();
              }
);
interactive("search-clipboard-contents-doublequoted", "Search in Google the content of the X clipboard (the selected text) as a fixed string",
              "find-url",
              $browser_object=
              function(I) {
                  return "duckduckgo \""+ read_from_x_primary_selection()+"\"";
              }

);
define_key(content_buffer_normal_keymap, "M-y", "search-clipboard-contents");
define_key(content_buffer_normal_keymap, "M-Y", "search-clipboard-contents-doublequoted");

//-----=====-----
//Bind Number Keys to Switch to Buffers 1-10
function define_switch_buffer_key (key, buf_num) {
    define_key(default_global_keymap, key,
               function (I) {
                   switch_to_buffer(I.window,
                                    I.window.buffers.get_buffer(buf_num));
               });
}
for (let i = 0; i < 10; ++i) {
    define_switch_buffer_key(String((i+1)%10), i);
}

// I think by the time kill_buffer_hook runs the buffer is gone so I
// patch kill_buffer

var kill_buffer_original = kill_buffer_original || kill_buffer;

var killed_buffer_urls = [];

kill_buffer = function (buffer, force) {
    if (buffer.display_uri_string) {
        killed_buffer_urls.push(buffer.display_uri_string);
    }

    kill_buffer_original(buffer,force);
};

interactive("restore-killed-buffer-url", "Loads url from a previously killed buffer",
            function restore_killed_buffer_url (I) {
                if (killed_buffer_urls.length !== 0) {                
                    var url = yield I.minibuffer.read(
                        $prompt = "Restore killed url:",
                        $completer = all_word_completer($completions = killed_buffer_urls),
                        $default_completion = killed_buffer_urls[killed_buffer_urls.length - 1],
                        $auto_complete = "url",
                        $auto_complete_initial = true,
                        $auto_complete_delay = 0,
                        $match_required);
                    
                    load_url_in_new_buffer(url);
                } else {
                    I.window.minibuffer.message("No killed buffer urls");
                }
            });

var user_agents = { "conkeror": "Mozilla/5.0 (X11; Linux x86_64; rv:8.0.1) " +
                    "Gecko/20100101 conkeror/1.0pre",
                    "chromium": "Mozilla/5.0 (X11; U; Linux x86_64; en-US) " +
                    "AppleWebKit/534.3 (KHTML, like Gecko) Chrome/6.0.472.63" +
                    " Safari/534.3",
                    "firefox": "Mozilla/5.0 (X11; Linux x86_64; rv:8.0.1) " +
                    "Gecko/20100101 Firefox/8.0.1",
                    "android": "Mozilla/5.0 (Linux; U; Android 2.2; en-us; " +
                    "Nexus One Build/FRF91) AppleWebKit/533.1 (KHTML, like " +
                    "Gecko) Version/4.0 Mobile Safari/533.1"};

var agent_completer = prefix_completer($completions = Object.keys(user_agents));

interactive("user-agent", "Pick a user agent from the list of presets",
            function(I) {
                var ua = (yield I.window.minibuffer.read(
                    $prompt = "Agent:",
                    $completer = agent_completer));
                set_user_agent(user_agents[ua]);
            });

interactive("qrcode", "Open QR code of current URL.",
            function qrcode(I) {
                I.window.content.location = 'http://chart.apis.google.com/chart?cht=qr&chs=300x300&chl=' +
                    encodeURIComponent(I.window.content.location.href);
            });
