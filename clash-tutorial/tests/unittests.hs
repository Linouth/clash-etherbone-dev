import Prelude

import Test.Tasty

-- import qualified Tests.Example.Project
-- import qualified Tests.TestPacketStream
import qualified Tests.Demo

main :: IO ()
main = defaultMain $ testGroup "."
  [ Tests.Demo.tests
  --   Tests.Example.Project.accumTests
  -- , Tests.TestPacketStream.tests
  ]
