-- | Contains types for dealing with 'ModuleBody's.
module FP15.Compiler.ModuleBody where
import FP15.Types
import FP15.Compiler.Types
import qualified Data.Map.Strict as M

-- | The empty 'ModuleBody'.
empty :: ModuleBody id fn fl ffn ffl
empty = Module M.empty M.empty M.empty M.empty

-- | The 'map' maps a 'ModuleBody' using 4 functions of each field.
map :: ( Map (id F) f -> Map (id1 F) f1
       , Map (id Fl) fl -> Map (id1 Fl) fl1
       , Map (id FOp) ffx -> Map (id1 FOp) ffx1
       , Map (id FlOp) flfx -> Map (id1 FlOp) flfx1
       ) -> ModuleBody id f fl ffx flfx -> ModuleBody id1 f1 fl1 ffx1 flfx1
map (mfs, mfls, mfFixes, mflFixes) Module { fs, fls, fFixes, flFixes }
  = Module { fs = mfs fs, fls = mfls fls
           , fFixes = mfFixes fFixes, flFixes = mflFixes flFixes }

-- | The 'combine' function combines two 'ModuleBody's using 4 functions for
-- each field.
combine
  :: ( Map (id1 F) f1 -> Map (id2 F) f2 -> Map (id F) f
     , Map (id1 Fl) fl1 -> Map (id2 Fl) fl2 -> Map (id Fl) fl
     , Map (id1 FOp) ffx1 -> Map (id2 FOp) ffx2 -> Map (id FOp) ffx
     , Map (id1 FlOp) flfx1 -> Map (id2 FlOp) flfx2 -> Map (id FlOp) flfx
     ) -> ModuleBody id1 f1 fl1 ffx1 flfx1
       -> ModuleBody id2 f2 fl2 ffx2 flfx2
       -> ModuleBody id f fl ffx flfx
combine (mfs, mfls, mfFixes, mflFixes)
        Module { fs = fs1, fls = fls1, fFixes = fFixes1, flFixes = flFixes1 }
        Module { fs = fs2, fls = fls2, fFixes = fFixes2, flFixes = flFixes2 }
  = Module { fs = mfs fs1 fs2
           , fls = mfls fls1 fls2
           , fFixes = mfFixes fFixes1 fFixes2
           , flFixes = mflFixes flFixes1 flFixes2 }
