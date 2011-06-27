{-# LANGUAGE ExtendedDefaultRules, OverloadedStrings, QuasiQuotes #-}
import Snapper
import qualified Text.Hamlet as H
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L

-- Routes for GET
get [] = do
    set "title" "Main Page"
    set "result" ""
    template "home"

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
    set "title" "Added"
    res $ addHeader "X-Powered-By" "Snap" -- ditto
    ua <- req $ getHeader "User-Agent"
    x <- param "x" 0
    y <- param "y" 0
    set "result" $ show (x+y)
    template "home"

post _ = pass

main = snapper routes{ _GET_ = get, _POST_ = post } $ do
    -- Inline templates
    html "home" [s|
<ul>
    <li><a href="hello/1/2/3">Hello</a></li>
    <li><a href="say/world">world,</a></li>
    <li><a href="echo?message=exclamation;foo=bar">(echo)!</a></li>
</ul>
<hr>
<form action=/add method=post>
    <input name=x value=$(_x)> + <input name=y value=$(_y)>
    <input type=submit value=Calculate!>
    <_result/>
</form>
|]
    html "layouts/default" [s|
<html>
  <head>
    <link rel="stylesheet" type="text/css" href="/screen.css" />
    <title><_title/></title>
  </head>
  <body>
    <div id="header">
      <h1>Snapper!</h1>
    </div>
    <div id="content">
      <content />
    </div>
    <div id="footer">
      <p><em>
      <a href="http://creativecommons.org/publicdomain/zero/1.0">CC0 Universal</a>:
      To the extent possible under law, 唐鳳 has waived all copyright and
      related or neighboring rights to the <b>Snapper</b> framework.
      </em></p>
    </div>
  </body>
</html>
|]
    -- External static directory, and dynamic template directory with .tpl/.xtpl files
    return ("static", "templates")

