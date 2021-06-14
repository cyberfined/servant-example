module Controllers.UserSpec (spec) where

import Controllers.Helper
import Factories.User

spec :: Spec
spec = withApp $ do
    describe "GET /api/user" $ do
        let expected tok = [json|
            { user: { username: "user"
                    , email: "user@example.com"
                    , bio: "Born. Lived. Died"
                    , image: "https://example.com/image0.png"
                    , token: #{tok}
                    }
            }
            |]

        it "returns current user" $ do
            setAppTime "2021-11-02 23:00:00"
            uid <- runStatement insertUser def
            token <- getJwtToken uid

            getAuth "/api/user" token "" `shouldRespondWith` (expected token)
