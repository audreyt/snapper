{-# LANGUAGE ExtendedDefaultRules, OverloadedStrings, QuasiQuotes #-}
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

post ["add"] = do
    x <- param "x"
    y <- param "y"
    text $ show (x+y)
post _ = pass

main = snapper routes{ _GET_ = get, _POST_ = post } $ do
    -- Inline templates
    html "main" [s|
<html><body>
    <a href="hello/1/2/3">Hello</a>
    <a href="say/world">world</a>
    <a href="echo?message=exclamation">!!!</a>
    <hr>
    <form action="add" method="post">
        <input name="x"> + <input name="y"> <input type="submit" value="=">
    </form>
</body></html>
|]
    -- External static directory, and dynamic template directory with .tpl/.xtpl files
    return ("static", "templates")

