--- Idea and name from an old program that used to hang
--- around Reed College, probably written by Graham Ross and
--- now apparently lost in the mists of time.
--- See
---   http://groups.google.com/group/net.sources/msg/8856593862fe22bd
--- for the one very vague reference I've found on the web.

--- Soundex is designed for names, but seems to work about the same
--- with other words.  I allow it to do arbitrary-length
--- codes instead of length 4 by default, to get better accuracy.

import Data.Soundex

main = interact (unwords . map soundexNARA . words)
