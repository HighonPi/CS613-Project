
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}


module TraverseAST
    (traverseAst
    )
where

import GHC.Prelude

import Data.Data hiding (Fixity)
import GHC.Data.Bag
import GHC.Types.Basic ( SourceText )
import GHC.Data.FastString
import GHC.Types.Name.Set
import GHC.Types.Name
import GHC.Core.DataCon
import GHC.Types.SrcLoc
import GHC.Hs
import GHC.Types.Var
import GHC.Unit.Module
import GHC.Utils.Outputable
import GHC.Hs.Dump (BlankSrcSpan(BlankSrcSpan, NoBlankSrcSpan))

import qualified Data.ByteString as B

traverseAst :: Data a => BlankSrcSpan -> a -> SDoc
traverseAst b a0 = blankLine $$ traverseAst' a0
  where
    traverseAst' :: Data a => a -> SDoc
    traverseAst' =
      generic
              `ext1Q` list
              `extQ` srcSpan
              `extQ` litt
              `extQ` bytestring
              `extQ` var
              `extQ` bagVar
              `ext2Q` located

      where generic :: Data a => a -> SDoc
            generic t = parens $ vcat (gmapQ traverseAst' t)

            bytestring :: B.ByteString -> SDoc
            bytestring = text . normalize_newlines . show

            list []    = brackets empty
            list [x]   = brackets (traverseAst' x)
            list (x1 : x2 : xs) =  (text "[" GHC.Utils.Outputable.<> traverseAst' x1)
                                $$ go x2 xs
              where
                go y [] = text "," GHC.Utils.Outputable.<> traverseAst' y GHC.Utils.Outputable.<> text "]"
                go y1 (y2 : ys) = (text "," GHC.Utils.Outputable.<> traverseAst' y1) $$ go y2 ys

            -- Eliminate word-size dependence
            litt :: HsLit GhcTc -> SDoc
            litt (HsWordPrim   s x) = numericLit "HsWord{64}Prim" x s
            litt (HsWord64Prim s x) = numericLit "HsWord{64}Prim" x s
            litt (HsIntPrim    s x) = numericLit "HsInt{64}Prim"  x s
            litt (HsInt64Prim  s x) = numericLit "HsInt{64}Prim"  x s
            litt l                  = generic l

            numericLit :: String -> Integer -> SourceText -> SDoc
            numericLit tag x s = braces $ hsep [ text tag
                                               , generic x
                                               , generic s ]

            srcSpan :: SrcSpan -> SDoc
            srcSpan ss = case b of
             BlankSrcSpan -> text "{ ss }"
             NoBlankSrcSpan -> braces $ char ' ' GHC.Utils.Outputable.<>
                             (hang (ppr ss) 1
                                   -- TODO: show annotations here
                                   (text ""))

            var  :: Var -> SDoc
            var v      = braces $ text "Var: " GHC.Utils.Outputable.<> ppr v

            bagVar    :: Bag (Located (HsBind GhcTc)) -> SDoc
            bagVar bg  =  braces $
                          text "Bag(Located (HsBind Var)):"
                       $$ (list . bagToList $ bg)

            located :: (Data b,Data loc) => GenLocated loc b -> SDoc
            located (L ss a) = parens $
                   case cast ss of
                        Just (s :: SrcSpan) ->
                          srcSpan s
                        Nothing -> text "nnnnnnnn"
                      $$ traverseAst' a

normalize_newlines :: String -> String
normalize_newlines ('\\':'r':'\\':'n':xs) = '\\':'n':normalize_newlines xs
normalize_newlines (x:xs)                 = x:normalize_newlines xs
normalize_newlines []                     = []





-- | The type constructor for queries
newtype Q q x = Q { unQ :: x -> q }

-- | Extend a generic query by a type-specific case
extQ :: ( Typeable a
        , Typeable b
        )
     => (a -> q)
     -> (b -> q)
     -> a
     -> q
extQ f g a = maybe (f a) g (cast a)

-- | Type extension of queries for type constructors
ext1Q :: (Data d, Typeable t)
      => (d -> q)
      -> (forall e. Data e => t e -> q)
      -> d -> q
ext1Q def ext = unQ ((Q def) `ext1` (Q ext))


-- | Type extension of queries for type constructors
ext2Q :: (Data d, Typeable t)
      => (d -> q)
      -> (forall d1 d2. (Data d1, Data d2) => t d1 d2 -> q)
      -> d -> q
ext2Q def ext = unQ ((Q def) `ext2` (Q ext))

-- | Flexible type extension
ext1 :: (Data a, Typeable t)
     => c a
     -> (forall d. Data d => c (t d))
     -> c a
ext1 def ext = maybe def id (dataCast1 ext)



-- | Flexible type extension
ext2 :: (Data a, Typeable t)
     => c a
     -> (forall d1 d2. (Data d1, Data d2) => c (t d1 d2))
     -> c a
ext2 def ext = maybe def id (dataCast2 ext)
