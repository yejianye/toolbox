tell application "Finder"
	set _cwd to POSIX path of ((folder of (front window)) as alias)
end tell

tell application "iTerm"
	activate
	tell last terminal
		launch session "Default"
		set _session to current session
		tell _session
			write text "pushd \"" & _cwd & "\""
		end tell
	end tell
end tell
