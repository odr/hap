{-# LANGUAGE LambdaCase, RecordWildCards, ScopedTypeVariables #-}
module Hap.Dictionary.Form where

import Hap.Dictionary.Import 
import Control.Lens(traverse, ix, (^.), (.~))
import qualified Data.Foldable as FD
import qualified Data.Traversable as TR
import Data.Monoid(Endo(..))

import Hap.Dictionary.Types
-- import Hap.Dictionary.Utils
import Hap.Dictionary.EntityPlus

entityPlusMForm :: (HasDictionary m e) 
        => EntityPlus m e -> Html -> MForm (HandlerT m IO) (FormResult (EntityPlus m e), WidgetT m IO ())
entityPlusMForm (ep0 :: EntityPlus m e) fragment = do
    (r,w) <- entityPlusForm ep0
    let widget = [whamlet|
$newline never
\#{fragment}
^{w}
|]  
    return (r,widget)  

entityPlusForm :: (HasDictionary m e) 
        => EntityPlus m e -> MForm (HandlerT m IO) (FormResult (EntityPlus m e), WidgetT m IO ())
entityPlusForm (ep0 :: EntityPlus m e) = epLtWForm (dFields (getDictionary :: Dictionary m e)) ep0
  where
{-    
    layoutToAForm :: (HasDictionary m e) 
            => Layout (DicField m e) -> EntityPlus m e -> AForm (HandlerT m IO) (EntityPlus m e)
                        (FormResult (EntityPlus m e), [FieldView m] -> [FieldView m])
    layoutToAForm lt =
        case lt of 
            Layout (DicField {..} :: DicField m e) -> case dfIndex of
                NormalField _ ef    
                    ->  aFormToForm . (epEntity . fieldLens ef) (fieldAForm ([] :: [e]) dfSettings . Just)
                RefField    _ _ n
                    -> (epRefs . ix n) (\(EntityRef ef (eps :: [EntityPlus m r])) -> EntityRef ef 
                                <$> traverse (layoutToForm $ dFields (getDictionary :: Dictionary m r)) eps) 

    dicLayoutForm :: (HasDictionary m e) 
        => Layout   (EntityPlus m e -> MForm (HandlerT m IO) 
                        (FormResult (EntityPlus m e), [FieldView m] -> [FieldView m])
                    )            
    dicLayoutForm = fmap (aFormToForm . fieldToAForm) $ dFields getDictionary

    layoutToWidget :: Layout   (EntityPlus m e -> MForm (HandlerT m IO) 
                        (FormResult (EntityPlus m e), [FieldView m] -> [FieldView m])
                    ) -> EntityPlus m e -> MForm  (HandlerT m IO) 
                        (FormResult (EntityPlus m e), WidgetT m IO ())
    layoutToWidget lt = case lt of
        Layout     f  -> f >=> renderDivs'
        Vertical   fs -> getForm True fs
        Horizontal fs -> getForm False fs
      where
        getForm isVert fs e0 
            = fmap (\(a,b,c) -> (fromMaybe def $ getLast a,c))
                $ foldM compose (FormSuccess (Last $ Just e0), e0,mempty) $ map layoutToWidget fs
          where
            compose (rl,e,w) f = do
                (r', w') <- f e
                let e' = case r' of 
                        FormSuccess se  -> se
                        _               -> e
                    rl' = rl <> fmap (Last . Just) r'
                let widget = [whamlet|
$newline never
^{w}
<div :isVert:.vertical :not $ isVert:.horizontal>
    ^{w'}
|]
                return (rl', e', widget)

    renderDivs' 
        :: MForm (HandlerT m IO) (FormResult (EntityPlus m e), [FieldView m] -> [FieldView m])
        -> MForm (HandlerT m IO) (FormResult (EntityPlus m e), WidgetT m IO ())
    renderDivs'  form = do
        (res, views') <- form
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

-}

    epLtWForm :: (HasDictionary m e) 
            => Layout (DicField m e) -> EntityPlus m e 
            -> MForm (HandlerT m IO) (FormResult (EntityPlus m e), WidgetT m IO ())
    epLtWForm lt (ep :: EntityPlus m e) = case lt of
        Layout (DicField {..} :: DicField m e) -> 
            case dfIndex of
                NormalField _ ef    
                    ->  render $ (epEntity . fieldLens ef) 
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
                                        $ traverse entityPlusForm eps
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
            (res,_,widgets) <- foldM compose def ls
                 -- fmap (second ($ [])) $ epLtsForm ls 
            let widget = [whamlet|
$newline never
$forall w <- reverse widgets
    <div :isVert:.vertical :not $ isVert:.horizontal>
        ^{w}
|]
            return (res, widget)
          where
            compose (r,e,ws) l = do
                (r',w') <- epLtWForm l e
                case r' of
                    FormSuccess e' -> return (r',e',w':ws)
                    _   -> return (r',e,w':ws)
{-
          where
            epLtsForm :: HasDictionary m e
                => [Layout (DicField m e)]
                -> MForm (HandlerT m IO) 
                        (FormResult (EntityPlus m e) , [FieldView m] -> [FieldView m])
            epLtsForm ls
                = fmap ((fmap getLast' . fst) *** appEndo) 
                        (foldM app ((FormSuccess lep0, lep0), mempty) ls)
              where
                getLast' :: (Default a) => Last a -> a
                getLast' = fromMaybe def . getLast

                lep0 = Last $ Just ep
                
                app acc@((_,lep),_) lt = fmap (acc <>) $ epLtForm lt lep
                
                epLtForm :: (HasDictionary m e) 
                        => Layout (DicField m e) -> Last (EntityPlus m e)
                        -> MForm (HandlerT m IO) 
                                ( (FormResult (Last (EntityPlus m e)), Last (EntityPlus m e))
                                , Endo [FieldView m]
                                )
                epLtForm l lep
                    = fmap ((fmap (Last . Just) &&& caseRes) *** Endo) 
                            ( epLtWForm l $ getLast' lep)
                  where
                    caseRes = \case
                        FormSuccess lep'    -> Last $ Just lep'
                        _                   -> lep
-}