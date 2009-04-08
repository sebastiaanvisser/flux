module OpenGL.ProggyOptiS (proggyOptiS) where

import Engine.Vector (Vector2 (..))
import Engine.Range (Range (..))
import Engine.Font

proggyOptiS :: Font
proggyOptiS =
  Font {
    ftName        = "ProggyOptiS"
  , ftWidth       = 16
  , ftHeight      = 12
  , ftGlyphWidth  = 6
  , ftGlyphHeight = 11
  , ftMap         = [
      IndexBlock 32 (V2 0 0 :~: V2 16 6) "textures/opti.rbga"
    ]
  }


