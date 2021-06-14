module Controllers.AuthSpec (spec) where

import Controllers.Helper
import Data.Aeson hiding  (json)
import Data.Int           (Int64)
import Data.Text          (Text)
import GHC.Generics
import Hasql.Statement

import Factories.User

import qualified Database.Hasqul as Hasqul

data SignResponse = SignResponse
    { signImage    :: !(Maybe Text)
    , signBio      :: !(Maybe Text)
    , signEmail    :: !Text
    , signUsername :: !Text
    , signToken    :: !Text
    } deriving Generic

instance FromJSON SignResponse where
    parseJSON = jsonRoot "user" $ genericParseJSON (jsonStrip "sign")

getUsersCount :: WaiSession TestContext Int64
getUsersCount = runStatement (Statement sql enc dec False) ()
  where sql = "SELECT COUNT(*) FROM users"
        enc = Hasqul.encode @()
        dec = Hasqul.decode @Int64

spec :: Spec
spec = withApp $ do
    describe "POST /signup" $ do
        context "with valid data" $ do
            let body = [json|
                { user: { username: "user"
                        , email: "user@example.com"
                        , password: "password"
                        }
                }|]

            it "returns user" $ do
                SResponse{..} <- post "/api/users" body
                numUsers <- getUsersCount
                let Just (SignResponse{..}) = decode simpleBody
                liftIO $ do
                    simpleStatus `shouldBe` ok200
                    signImage `shouldBe` Nothing
                    signBio `shouldBe` Nothing
                    signEmail `shouldBe` "user@example.com"
                    signUsername `shouldBe` "user"
                    signToken `shouldNotBe` ""
                    numUsers `shouldBe` 1

        context "with used username" $ do
            let body = [json|
                { user: { username: "user"
                        , email: "uniq@example.com"
                        , password: "password"
                        }
                }|]
                err = [json|{errors: {username: ["is already used"]}}|]
                existedUser = def { userLogin = Login "user" }

            it "returns error" $ do
                void $ runStatement insertUser existedUser

                SResponse{..} <- post "/api/users" body
                numUsers <- getUsersCount
                liftIO $ do
                    simpleStatus `shouldBe` unprocessableEntity422
                    simpleBody `shouldBe` err
                    numUsers `shouldBe` 1

        context "with used email" $ do
            let body = [json|
                { user: { username: "uniq"
                        , email: "user@example.com"
                        , password: "password"
                        }
                }|]
                err = [json|{errors: {email: ["is already used"]}}|]
                existedUser = def { userEmail = Email "user@example.com" }

            it "returns error" $ do
                void $ runStatement insertUser existedUser

                SResponse{..} <- post "/api/users" body
                numUsers <- getUsersCount
                liftIO $ do
                    simpleStatus `shouldBe` unprocessableEntity422
                    simpleBody `shouldBe` err
                    numUsers `shouldBe` 1

        context "with invalid email" $ do
            let body = [json|
                { user: { username: "uniq"
                        , email: "invalid"
                        , password: "password"
                        }
                }|]
                err = [json|{errors: {email: ["is invalid"]}}|]

            it "returns error" $ do
                SResponse{..} <- post "/api/users" body
                numUsers <- getUsersCount
                liftIO $ do
                    simpleStatus `shouldBe` unprocessableEntity422
                    simpleBody `shouldBe` err
                    numUsers `shouldBe` 0

        context "with short password" $ do
            let body = [json|
                { user: { username: "user"
                        , email: "user@example.com"
                        , password: "12345"
                        }
                }|]
                err = [json|{errors: {password: ["is too short"]}}|]

            it "returns error" $ do
                SResponse{..} <- post "/api/users" body
                numUsers <- getUsersCount
                liftIO $ do
                    simpleStatus `shouldBe` unprocessableEntity422
                    simpleBody `shouldBe` err
                    numUsers `shouldBe` 0

    describe "POST /signin" $ do
        context "with valid data" $ do
            let body = [json|
                { user: { email: "user@example.com"
                        , password: "password"
                        }
                }|]

            it "returns user" $ do
                void $ runStatement insertUser def

                SResponse{..} <- post "/api/users/login" body
                let Just (SignResponse{..}) = decode simpleBody
                liftIO $ do
                    simpleStatus `shouldBe` ok200
                    signImage `shouldBe` Just "https://example.com/image0.png"
                    signBio `shouldBe` Just "Born. Lived. Died"
                    signEmail `shouldBe` "user@example.com"
                    signUsername `shouldBe` "user"
                    signToken `shouldNotBe` ""

        context "with invalid data" $ do
            let body = [json|
                { user: { email: "user@example.com"
                        , password: "password123"
                        }
                }|]
                err = [json|{errors: ["Wrong email or password"]}|]

            it "returns error" $ do
                void $ runStatement insertUser def

                SResponse{..} <- post "/api/users/login" body
                liftIO $ do
                    simpleStatus `shouldBe` unauthorized401
                    simpleBody `shouldBe` err

    describe "authentication" $ do
        let body = [json|
            { user: { email: "user@example.com"
                    , password: "password"
                    }
            }|]

        context "with valid token" $ do
            it "returns 200 ok" $ do
                void $ runStatement insertUser def

                signinResponse <- post "/api/users/login" body
                let Just (SignResponse{..}) = decode (simpleBody signinResponse)

                getAuth "/api/user" signToken "" `shouldRespondWith` 200

        context "when token has expired" $ do
            it "returns error" $ do
                void $ runStatement insertUser def
                setAppTime "2021-10-01 18:30:00"

                signinResponse <- post "/api/users/login" body
                let Just (SignResponse{..}) = decode (simpleBody signinResponse)

                setAppTime "2021-10-20 18:30:11"
                getAuth "/api/user" signToken "" `shouldRespondWith` 401
