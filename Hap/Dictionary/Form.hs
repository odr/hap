{-# LANGUAGE LambdaCase, RecordWildCards, ScopedTypeVariables #-}
module Hap.Dictionary.Form where

import Hap.Dictionary.Import 
import Control.Lens(traverse, ix, (^.), (.~))
import qualified Data.Traversable as TR

import Hap.Dictionary.Types
-- import Hap.Dictionary.EntityPlus

entityPlusMForm :: (HasDictionary m e) 
        => EntityPlus m e -> Html -> MForm (HandlerT m IO) (FormResult (EntityPlus m e), WidgetT m IO ())
entityPlusMForm (ep0 :: EntityPlus m e) fragment = do
    (r,w) <- entityPlusForm (Nothing :: Maybe (EntityField e (Key e))) ep0
    
    let widget = do
        toWidget [cassius|
.vertical
    display: block
.horizontal
        display: inline-block 
|]
        [whamlet|
$newline never
\#{fragment}
^{w}
|]  
    return (r,widget)  

entityPlusForm :: (HasDictionary m e, ForeignKey a e e') 
        => Maybe a -> EntityPlus m e 
        -> MForm (HandlerT m IO) (FormResult (EntityPlus m e), WidgetT m IO ())
entityPlusForm mbFK (ep0 :: EntityPlus m e) = epLtWForm (dFields (getDictionary :: Dictionary m e)) ep0
  where
    epLtWForm :: (HasDictionary m e) 
            => Layout (DicField m e) -> EntityPlus m e 
            -> MForm (HandlerT m IO) (FormResult (EntityPlus m e), WidgetT m IO ())
    epLtWForm lt (ep :: EntityPlus m e) = case lt of
        Layout (DicField {..} :: DicField m e) -> 
            case dfIndex of
                NormalField _ ef    
                    | maybe False (\a -> eqFK a ef) mbFK   
                                -> return (FormSuccess ep, mempty)
                    | otherwise -> render $ (epEntity . fieldLens ef) 
                                        (fieldAForm ([] :: [e]) dfSettings . Just) 
                                        ep
                RefField    _ _ n
                    -> do
                        let refs = ep ^. epRefs
                            ref = refs !! n
                        case  ref of
                            (EntityRef ef (eps :: [EntityPlus m r])) ->
                                renderRef n
                                    $ fmap ((fmap (EntityRef ef) . TR.sequenceA *** mconcat) . unzip) 
                                        $ traverse (entityPlusForm $ Just ef) eps
        Vertical ls     -> epLtsWForm True ls
        Horizontal ls   -> epLtsWForm False ls
      where
        render form = do
            (res, views') <- aFormToForm form
            let views = views' []
            let widget = [whamlet|
$newline never
$forall view <- views
    <div :fvRequired view:.required :not $ fvRequired view:.optional>
        <label for=#{fvId view}>#{fvLabel view}
        $maybe tt <- fvTooltip view
            <div .tooltip>#{tt}
        ^{fvInput view}
        $maybe err <- fvErrors view
            <div .errors>#{err}
    |]
            return (res, widget)

        renderRef :: Int -> MForm (HandlerT m IO) (FormResult (EntityRef m e), WidgetT m IO ())
            -> MForm (HandlerT m IO) (FormResult (EntityPlus m e), WidgetT m IO ())
        renderRef n form = do
            (res', w') <- form
            let res = fmap (\er -> (epRefs . ix n .~ er) ep) res'
            return (res, w')

        epLtsWForm :: HasDictionary m e 
            => Bool -> [Layout (DicField m e)] -> MForm (HandlerT m IO) (FormResult (EntityPlus m e), WidgetT m IO ())
        epLtsWForm isVert ls = do
            (lres,_,widgets) <- foldM compose (FormSuccess $ Last $ Just ep, ep, mempty) ls
            let widget = [whamlet|
$newline never
$forall w <- reverse widgets
    <div :isVert:.vertical :not $ isVert:.horizontal>
        ^{w}
|]
            return (fmap (fromMaybe def . getLast) lres, widget)
          where
            compose (lr,e,ws) l = do
                (r',w') <- epLtWForm l e
                case r' of
                    FormSuccess e' -> return (lr <> fmap (Last . Just) r', e', w':ws)
                    _   -> return (lr <> fmap (Last . Just) r', e, w':ws)
