{-# LANGUAGE OverloadedStrings, FlexibleInstances, MultiParamTypeClasses, TypeSynonymInstances, OverloadedStrings, Rank2Types, ExistentialQuantification #-}
module Snapper (
    Routes(..),
    routes, snapper, html, xhtml,
    set, var, hasParam, param, template, text,
    contentType,

    status, header, res, req, hamlet, h, halt, layout,

    module Snap.Types,
    module Data.String.QQ,
    module Snap.Util.FileServe,
    module Control.Applicative
) where
import Snap.Extension.Server
import Snap.Types
import Snap.Util.FileServe
import Data.String.QQ
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import Control.Applicative
import qualified Data.ByteString.UTF8 as U
import Data.String (IsString(fromString))
import Data.Map as M
import Snap.Extension
import Snap.Extension.Heist.Impl
import Snap.Types
import Text.Templating.Heist
import qualified Data.Text as T
import qualified Data.Text.Encoding as E
import qualified Text.XmlHtml as X
import qualified Control.Monad.Writer as W
import Snap.Util.Readable
import Control.Monad.Reader.Class (asks, local)
import Control.Monad.IO.Class (liftIO, MonadIO)
import qualified Text.Hamlet as H
import qualified Data.HashTable.IO as HT

data Routes m = MonadSnap m => Routes
    { _GET_, _POST_, _HEAD_, _DELETE_, _PUT_ :: [String] -> m ()
    , _before_, _after_ :: m ()
    }

contentType :: MonadSnap m => ByteString -> m ()
contentType = modifyResponse . setContentType

routes :: MonadSnap m => Routes m
routes = Routes
    nil nil nil nil nil
    (return ()) (return ())
    where
    nil = const pass

snapper routes templates = quickHttpServe quickInit quickSite
    where
    quickInit = do
        ts <- heistInitializer dynamic applyTemplates
        vs <- io $ HT.new
        return $ SnapperState{ templateState = ts, variableState = vs }
    ((static, dynamic), templateMap) = W.runWriter templates
    applyTemplates :: Monad m => TemplateState m -> TemplateState m
    applyTemplates = foldl 
        (\f (k, v) -> f . addTemplate k v)
        id 
        (M.toList templateMap)
    quickSite = do
        req <- getRequest
        vs <- io $ HT.fromList
            [ ("_" `T.append` E.decodeUtf8 k, E.decodeUtf8 (B.intercalate " " vs))
            | (k, vs) <- M.toList $ rqParams req
            ]
        local (\s -> s{ variableState = vs }) $ do
            _before_ routes
            pathInfo <- (maybe pass return . urlDecode . rqPathInfo) req
            let args = U.toString <$> B.splitWith (== 0x2F) pathInfo
            case rqMethod req of 
                GET -> (_GET_ routes) args <|> template (U.toString pathInfo) <|> serveDirectory static
                HEAD -> (_HEAD_ routes) args <|> template (U.toString pathInfo) <|> serveDirectory static
                POST -> (_POST_ routes) args
                PUT -> (_PUT_ routes) args
                DELETE -> (_DELETE_ routes) args
                _ -> pass
            _after_ routes

h = H.hamlet

hamlet :: ByteString -> H.Html -> W.Writer (M.Map ByteString Template) ()
hamlet k = html k . B.concat . L.toChunks . H.renderHtml

html :: ByteString -> ByteString -> W.Writer (M.Map ByteString Template) ()
html k v = case X.parseHTML (U.toString k) v of
    Left err  -> fail err
    Right doc -> W.tell $ M.singleton k (X.docContent doc)

xhtml :: ByteString -> ByteString -> W.Writer (M.Map ByteString Template) ()
xhtml k v = case X.parseXML (U.toString k) v of
    Left err  -> fail err
    Right doc -> W.tell $ M.singleton k (X.docContent doc)

var :: T.Text -> Snapper String
var key = do
    vs  <- asks variableState
    val <- io $ HT.lookup vs key
    return $ maybe "" T.unpack val

set :: T.Text -> String -> Snapper ()
set key val = do
    vs <- asks variableState
    io $ HT.insert vs key (T.pack val)

hasParam k = do
    x <- getParam k
    return $ maybe False (const True) x

param :: (MonadSnap m, Readable a) => ByteString -> m a
param k = do
    x <- getParam k
    maybe pass fromBS x

type Snapper = SnapExtend SnapperState
data SnapperState = SnapperState
    { templateState :: !(HeistState Snapper)
    , variableState :: !(HT.BasicHashTable T.Text T.Text)
    }

header k v = modifyResponse (addHeader k v)

status :: MonadSnap m => Int -> m ()
status = modifyResponse . setResponseCode

text :: MonadSnap m => String -> m ()
text = writeBS . U.fromString

layout :: String -> Snapper ()
layout = set ""

template :: String -> Snapper ()
template name = do
    vs <- asks variableState
    layout <- io $ HT.lookup vs ""
    args <- io $ HT.toList vs
    let layoutTemplate = "layout/" `T.append` maybe "default" id layout
    let applyLayout nodes = return [X.Element "apply" [("template", layoutTemplate)] nodes]
        hookLayout state
            | hasTemplate (E.encodeUtf8 layoutTemplate) state
            = addPreRunHook applyLayout state
            | otherwise
            = state
    heistLocal (hookLayout . bindStrings args) (render $ U.fromString name)

instance HasHeistState Snapper SnapperState where
    getHeistState     = templateState
    setHeistState s a = a { templateState = s }

res :: MonadSnap m => (Response -> Response) -> m ()
res = modifyResponse

req :: MonadSnap m => (Request -> a) -> m a
req = withRequest . (return .)

halt :: MonadSnap m => m ()
halt = finishWith =<< getResponse

io :: MonadIO m => IO a -> m a
io = liftIO
