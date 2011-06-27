{-# LANGUAGE ExtendedDefaultRules, OverloadedStrings, QuasiQuotes #-}
import Snapper
import qualified Text.Hamlet as H
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L

-- Routes for GET
get [] = do
    set "Title" "Main Page"
    template "main"

get ["say", msg] = do
    set "message" msg
    template "echo"

get ("hello":xs) = do
    contentType "text/plain"
    text $ "Hello: " ++ unwords xs

get _ = pass

-- Routes for POST
post ["add"] = do
    status 201
    header "X-Powered-By" "Snapper"
    res $ addHeader "X-Powered-By" "Snap" -- ditto
    ua <- req $ getHeader "User-Agent"
    x <- param "x"
    y <- param "y"
    text $ show (x+y)

post _ = pass

main = snapper routes{ _GET_ = get, _POST_ = post } $ do
    -- Inline templates
    html "main" [s|
<ul>
    <li><a href="hello/1/2/3">Hello</a></li>
    <li><a href="say/world">world,</a></li>
    <li><a href="echo?message=exclamation;foo=bar">(echo)!</a></li>
</ul>
<hr>
<form action=add method=post>
    <input name=x> + <input name=y>
    <input type=submit value=Calculate!>
</form>
|]
    html "layout/default" [s|
<html>
  <head>
    <link rel="stylesheet" type="text/css" href="/screen.css" />
    <title><Title/></title>
  </head>
  <body>
    <div id="header">
      <h1>XYZ Inc.</h1>
    </div>
    <div id="content">
      <content />
    </div>
    <div id="footer">
      <p>Copyright XYZ Inc</p>
    </div>
  </body>
</html>
|]
    -- External static directory, and dynamic template directory with .tpl/.xtpl files
    return ("static", "templates")

