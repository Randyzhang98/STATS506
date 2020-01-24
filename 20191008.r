> paste("a", "b")
[1] "a b"
> paste0("a", "b")
[1] "ab"
> sprintf('%s%i', "3,30 sd", 0)
[1] "3,30 sd0"
> sprintf('%s%i', "3,30 sd", "0")
Error in sprintf("%s%i", "3,30 sd", "0") :
  invalid format '%i'; use format %s for character objects

# Regular Expression

#inside [], ^ means "not" (!)

> cat('.')
.
> cat('\\.')
\.
> cat('\.')
Error: '\.' is an unrecognized escape in character string starting "'\."
> cat('\\.')
\.

# Matched values can be grouped using parentheses () and referred back to in the order they appear using a back reference \\1