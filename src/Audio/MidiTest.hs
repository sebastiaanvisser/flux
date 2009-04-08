module Audio.MidiTest where

import qualified Codec.Midi as Midi

main = do
  (Right midi) <- Midi.importFile "../midi/theamazingtransplant.mid"
  let (Midi.TicksPerBeat tpb) = Midi.timeDiv midi
      tracks = Midi.tracks midi
  print (length $ tracks )
  mapM_ (print . snd) (tracks !! 1)
