#!/bin/bash
entry=$(cat | sed 's/"/\\"/g')
emacsclient --no-wait --eval '(my/org-journal-new-entry "${entry}")'
