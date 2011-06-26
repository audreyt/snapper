{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}
import Snapper

-- Route definitions
get [] = tmpl "main"
get ["say", msg] = do
    set "message" msg
    tmpl "echo"
get ("hello":xs) = do
    mime "text/plain"
    text $ "Hello: " ++ unwords xs
get _ = pass

main = snapper routes{ _GET_ = get } $ do
    -- Inline templates
    html "main" [s|
<html><body>
    <a href="hello/1/2/3">Hello</a>
    <a href="say/world">world</a>
    <a href="echo?message=exclamation">!!!</a>
</body></html>
|]
    -- External static directory, and dynamic template directory with .tpl/.xtpl files
    return ("static", "templates")

