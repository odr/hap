{-# LANGUAGE LambdaCase, RecordWildCards, ScopedTypeVariables #-}
module Hap.Dictionary.Form where

import Hap.Dictionary.Import 
import Control.Lens(traverse, ix)
import qualified Data.Foldable as FD
import Data.Monoid(Endo(..))

import Hap.Dictionary.Types
-- import Hap.Dictionary.Utils
import Hap.Dictionary.EntityPlus

entityPlusAForm :: (HasDictionary m e) 
        => EntityPlus m e -> AForm (HandlerT m IO) (EntityPlus m e)
entityPlusAForm (ep0 :: EntityPlus m e) = epLtAForm (dFields (getDictionary :: Dictionary m e)) ep0
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

    epLtAForm :: (HasDictionary m e) 
            => Layout (DicField m e) -> EntityPlus m e -> AForm (HandlerT m IO) (EntityPlus m e)
    epLtAForm lt (ep :: EntityPlus m e) = case lt of
                Layout (DicField {..} :: DicField m e) -> 
                    case dfIndex of
                        NormalField _ ef    
                            ->  (epEntity . fieldLens ef) (fieldAForm ([] :: [e]) dfSettings . Just) ep
                        RefField    _ _ n
                            -> (epRefs . ix n) ((\(EntityRef ef (eps :: [EntityPlus m r])) -> 
                                EntityRef ef <$> traverse entityPlusAForm eps) ) ep
                Vertical ls     -> epLtsAForm ls
                Horizontal ls   -> epLtsAForm ls
      where
        epLtsAForm :: HasDictionary m e 
            => [Layout (DicField m e)] -> AForm (HandlerT m IO) (EntityPlus m e)
        epLtsAForm ls = formToAForm (fmap (second ($ [])) $ epLtsForm ls )
          where
            epLtsForm :: HasDictionary m e
                => [Layout (DicField m e)]
                -> MForm (HandlerT m IO) 
                        ( FormResult (EntityPlus m e) 
                        , [FieldView m] -> [FieldView m]
                        )
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
                            ( aFormToForm $ epLtAForm l $ getLast' lep)
                  where
                    caseRes = \case
                        FormSuccess lep'    -> Last $ Just lep'
                        _                   -> lep
