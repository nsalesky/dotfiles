function magit
emacsclient -c --eval "(progn (magit-status) (delete-other-windows))"
end
