{-# LANGUAGE OverloadedStrings, FlexibleInstances, MultiParamTypeClasses, TypeSynonymInstances, OverloadedStrings, Rank2Types, ExistentialQuantification #-}
module Snapper (
    Routes(..),
    routes, snapper, html, xhtml,
    set, sets, hasParam, param, tmpl, text, mime,

    status, header, res, req, hamlet, h,

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
import qualified Text.Hamlet as H

data Routes m = MonadSnap m => Routes
    { _GET_, _POST_, _HEAD_, _DELETE_, _PUT_ :: [String] -> m () }

mime :: MonadSnap m => ByteString -> m ()
mime = modifyResponse . setContentType

routes :: MonadSnap m => Routes m
routes = Routes (const pass) (const pass) (const pass) (const pass) (const pass)

snapper routes templates = quickHttpServe quickInit $ quickSite
    where
    quickInit = ApplicationState <$> heistInitializer dynamic applyTemplates
    ((static, dynamic), templateMap) = W.runWriter templates
    applyTemplates :: Monad m => TemplateState m -> TemplateState m
    applyTemplates = foldl 
        (\f (k, v) -> f . addTemplate k v)
        id 
        (M.toList templateMap)
    quickSite = do
        req <- getRequest
        p <- (maybe pass return . urlDecode . rqPathInfo) req
        let args = U.toString <$> B.splitWith (== 0x2F) p
        case rqMethod req of 
            GET -> (_GET_ routes) args <|> tmpl (U.toString p) <|> serveDirectory static
            HEAD -> (_HEAD_ routes) args <|> tmpl (U.toString p) <|> serveDirectory static
            POST -> (_POST_ routes) args
            PUT -> (_PUT_ routes) args
            DELETE -> (_DELETE_ routes) args
            _ -> pass

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

set k v = modifyRequest $ rqSetParam k [U.fromString v]

sets kvs = mapM_ (uncurry set) kvs

hasParam k = do
    x <- getParam k
    return $ maybe False (const True) x

param :: (MonadSnap m, Readable a) => ByteString -> m a
param k = do
    x <- getParam k
    maybe pass fromBS x

type Application = SnapExtend ApplicationState
newtype ApplicationState = ApplicationState { templateState :: HeistState Application }


header k v = modifyResponse (addHeader k v)

status :: MonadSnap m => Int -> m ()
status = modifyResponse . setResponseCode

text :: MonadSnap m => String -> m ()
text = writeBS . U.fromString

tmpl name = do
    params <- getParams
    let args = [ (E.decodeUtf8 k, E.decodeUtf8 (B.intercalate " " vs)) | (k, vs) <- M.toList params ]
    heistLocal (bindStrings args) (render $ U.fromString name)

instance HasHeistState Application ApplicationState where
    getHeistState     = templateState
    setHeistState s a = a { templateState = s }

res :: MonadSnap m => (Response -> Response) -> m ()
res = modifyResponse

req :: MonadSnap m => (Request -> a) -> m a
req = withRequest . (return .)
