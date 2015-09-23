module Handler.Club where

import Import

import Yesod.Form.Bootstrap3

import Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import Network.Mail.Mime
import Text.Shakespeare.Text (stext)
import qualified Data.Text.Lazy.Encoding

import Forms

getClubR :: Handler Html
getClubR = do
  -- ((_, formWidget), formEnctype) <- runFormPost joinForm
  let days, months, years :: [Int]
      days = [1..31]
      months = [1..12]
      years = [1915..2015]
  defaultLayout $ do
    $(widgetFile "banner")
    let sidebar = $(widgetFile "sidebar")
    $(widgetFile "club")

postClubR :: Handler Html
postClubR = do
  result <- runInputPost $ Member
    <$> ireq textField "firstname"
    <*> ireq textField "lastname"
    <*> ireq textField "address"
    <*> ireq intField "zip-code"
    <*> ireq textField "city"
    <*> ireq textField "phone"
    <*> ireq textField "email"
    <*> ireq (radioFieldList sexOptions) "sex"
    <*> ireq (selectFieldList dayOptions) "day"
    <*> ireq (selectFieldList monthOptions) "month"
    <*> ireq (selectFieldList yearOptions) "year"
    <*> iopt intField "pdga"
    <*> ireq (radioFieldList membershipOptions) "membership"
    <*> ireq (radioFieldList licenseOptions) "license"
    <*> ireq checkBoxField "disc"
    <*> ireq checkBoxField "magazine"
    <* ireq (checkCheck textField) "check"
  liftIO $ print result
  liftIO $ sendJoinMail result
  liftIO $ sendPaymentInfoMail result
  redirect ClubR

sendPaymentInfoMail :: Member -> IO ()
sendPaymentInfoMail member = do
  liftIO $ renderSendMail (emptyMail $ Address (Just "Tampereen Frisbeeseura") "jasenvastaava@tfs.fi")
    { mailTo = [Address Nothing (email member)]
    , mailHeaders =
        [ ("Subject", "Liittyminen Tampereen Frisbeeseuraan")
        ]
    , mailParts = [[textPart, htmlPart]]
    }
  where
    name = firstName member ++ " " ++ lastName member
    textPart = Part
      { partType = "text/plain; charset=utf-8"
      , partEncoding = None
      , partFilename = Nothing
      , partContent = Data.Text.Lazy.Encoding.encodeUtf8
          [stext|
            Tilinumero: FI17 8330 0710 4436 19
            Saaja: Tampereen Frisbeeseura
            Viestiin: Jäsenmaksu 2016, #{name}.
            Summa: #{show $ countSum member} €
          |]
      , partHeaders = []
      }
    htmlPart = Part
      { partType = "text/html; charset=utf-8"
      , partEncoding = None
      , partFilename = Nothing
      , partContent = renderHtml
          [shamlet|
            <p>Tilinumero: FI17 8330 0710 4436 19
            <p>Saaja: Tampereen Frisbeeseura
            <p>Viestiin: Jäsenmaksu 2016, #{name}.
            <p>Summa: #{show $ countSum member} €
          |]
      , partHeaders = []
      }

sendJoinMail :: Member -> IO ()
sendJoinMail member@Member {..} = do
  let name = firstName ++ " " ++ lastName
  liftIO $ renderSendMail (emptyMail $ Address (Just "Tampereen Frisbeeseura") "lomake@tfs.fi")
    { mailTo = [Address Nothing "jasenvastaava@tfs.fi"]
    , mailHeaders =
        [ ("Subject", "Seuraan liittyminen, " ++ name)
        , ("Reply-To", email)
        ]
    , mailParts = [[textPart, htmlPart]]
    }
  where
    name = firstName ++ " " ++ lastName
    textPart = Part
      { partType = "text/plain; charset=utf-8"
      , partEncoding = None
      , partFilename = Nothing
      , partContent = Data.Text.Lazy.Encoding.encodeUtf8
          [stext|
            #{name}
            #{address}
            #{show zipCode}
            #{city}
            #{phone}
            #{email}
            #{showSex sex}
            #{day}.#{month}.#{year}
            #{showPdga mpdga}
            #{showMembership membership}
            #{showLicense membership license}
            $if disc
              Jäsenkiekko: Kyllä
            $else
              Jäsenkiekko: Ei
            $if magazine
              Discgolfer-lehti: Kyllä
            $else
              Discgolfer-lehti: Ei
            Summa: #{show $ countSum member} €
          |]
      , partHeaders = []
      }
    htmlPart = Part
      { partType = "text/html; charset=utf-8"
      , partEncoding = None
      , partFilename = Nothing
      , partContent = renderHtml
          [shamlet|
            <p>#{show member}
          |]
      , partHeaders = []
      }

showMembership :: Membership -> String
showMembership Adult = "Jäsenmaksu, aikuinen"
showMembership Junior = "Jäsenmaksu, juniori"

showLicense :: Membership -> License -> String
showLicense _ NoLicense = "Ei lisenssiä"
showLicense Adult A = "A-lisenssi, aikuinen"
showLicense Adult B = "B-lisenssi, aikuinen"
showLicense Junior A = "A-lisenssi, juniori"
showLicense Junior B = "B-lisenssi, juniori"

showPdga :: Maybe Int -> String
showPdga Nothing = "PDGA-numero: -"
showPdga (Just pdga) = "PDGA-numero: " ++ show pdga

showSex :: Sex -> String
showSex Male = "Mies"
showSex Female = "Nainen"

data Member = Member
  { firstName :: Text
  , lastName :: Text
  , address :: Text
  , zipCode :: Int
  , city :: Text
  , phone :: Text
  , email :: Text
  , sex :: Sex
  , day :: Int
  , month :: Int
  , year :: Int
  , mpdga :: Maybe Int
  , membership :: Membership
  , license :: License
  , disc :: Bool
  , magazine :: Bool
  } deriving (Show)

countSum :: Member -> Int
countSum Member {..} =
  (msSum membership) + (licenseSum membership license) + (discSum disc) + (magazineSum magazine)
  where
    msSum :: Membership -> Int
    msSum Adult = 15
    msSum Junior = 5
    licenseSum :: Membership -> License -> Int
    licenseSum _ NoLicense = 0
    licenseSum Adult A = 55
    licenseSum Adult B = 20
    licenseSum Junior A = 30
    licenseSum Junior B = 10
    discSum :: Bool -> Int
    discSum True = 10
    discSum False = 0
    magazineSum :: Bool -> Int
    magazineSum True = 30
    magazineSum False = 0

dayOptions :: [(Text, Int)]
dayOptions = map (\d -> (tshow d, d)) [1..31]

monthOptions :: [(Text, Int)]
monthOptions = map (\m -> (tshow m, m)) [1..12]

yearOptions :: [(Text, Int)]
yearOptions = map (\y -> (tshow y, y)) [1915..2015]

sexOptions :: [(Text, Sex)]
sexOptions = [("male", Male), ("female", Female)]

membershipOptions :: [(Text, Membership)]
membershipOptions = [("adult", Adult), ("junior", Junior)]

licenseOptions :: [(Text, License)]
licenseOptions =
  [ ("no-license", NoLicense)
  , ("adult", A)
  , ("junior", A)
  , ("adult", B)
  , ("junior", B)
  ]

data Sex = Male | Female
  deriving (Show, Eq)

data Membership = Adult | Junior
  deriving (Show, Eq)

data License = NoLicense | A | B
  deriving (Show, Eq)

-- bot checking
checkCheck :: Field Handler Text -> Field Handler Text
checkCheck = checkBool (\v -> v == "5") MsgCheckError

-- joinForm :: Form Member
-- joinForm extra = do
--   mr <- getMessageRender
--   (firstNameRes, firstNameView) <- mreq textField
--     (withPlaceholder (mr MsgFirstName) $ bfs MsgFirstName) Nothing
--   (lastNameRes, lastNameView) <- mreq textField
--     (withPlaceholder (mr MsgLastName) $ bfs MsgLastName) Nothing
--   (addressRes, addressView) <- mreq textField
--     (withPlaceholder (mr MsgAddress) $ bfs MsgAddress) Nothing
--   (zipCodeRes, zipCodeView) <- mreq intField
--     (withPlaceholder (mr MsgZipCode) $ bfs MsgZipCode) Nothing
--   (cityRes, cityView) <- mreq textField
--     (withPlaceholder (mr MsgCity) $ bfs MsgCity) Nothing
--   (phoneRes, phoneView) <- mreq intField
--     (withPlaceholder (mr MsgPhone) $ bfs MsgPhone) Nothing
--   (emailRes, emailView) <- mreq emailField
--     (withPlaceholder (mr MsgEmail) $ bfs MsgEmail) Nothing
--   (sexRes, sexView) <- mreq (radioField sexOptions)
--     (withPlaceholder (mr MsgSex) $ bfs MsgSex) Nothing
--   (dobRes, dobView) <- mreq dayField
--     (withPlaceholder (mr MsgDateOfBirth) $ bfs MsgDateOfBirth) Nothing
--   (pdgaRes, pdgaView) <- mopt intField
--     (withPlaceholder (mr MsgPDGANumber) $ bfs MsgPDGANumber) Nothing
--   (msRes, msView) <- mreq (radioField membershipOptions)
--     (withPlaceholder (mr MsgMembership) $ bfs MsgMembership) Nothing
--   (licenseRes, licenseView) <- mopt (radioField licenseOptions)
--     (withPlaceholder (mr MsgMembership) $ bfs MsgMembership) Nothing
--   (msDiscRes, msDiscView) <- mreq checkBoxField
--     (withPlaceholder (mr MsgMembershipDisc) $ bfs MsgMembershipDisc) Nothing
--   (magazineRes, magazineView) <- mreq checkBoxField
--     (withPlaceholder (mr MsgMagazine) $ bfs MsgMagazine) Nothing
--   (checkRes, checkView) <- mreq (checkCheck textField)
--     (withPlaceholder (mr MsgCheck) $ bfs MsgCheck) Nothing
--   let result = Member
--                 <$> firstNameRes
--                 <*> lastNameRes
--                 <*> addressRes
--                 <*> zipCodeRes
--                 <*> cityRes
--                 <*> phoneRes
--                 <*> emailRes
--                 <*> sexRes
--                 <*> dobRes
--                 <*> pdgaRes
--                 <*> msRes
--                 <*> licenseRes
--                 <*> msDiscRes
--                 <*> magazineRes
--   let widget = [whamlet|
--         #{extra}
--         <div .form-group>
--           <label .control-label>^{fvLabel firstNameView}
--           ^{fvInput firstNameView}
--         <div .form-group>
--           <label .control-label>^{fvLabel lastNameView}
--           ^{fvInput lastNameView}
--         <div .form-group>
--           <label .control-label>^{fvLabel addressView}
--           ^{fvInput addressView}
--         <div .form-group>
--           <label .control-label>^{fvLabel zipCodeView}
--           ^{fvInput zipCodeView}
--         <div .form-group>
--           <label .control-label>^{fvLabel cityView}
--           ^{fvInput cityView}
--         <div .form-group>
--           <label .control-label>^{fvLabel phoneView}
--           ^{fvInput phoneView}
--         <div .form-group>
--           <label .control-label>^{fvLabel emailView}
--           ^{fvInput emailView}

--         <div .form-group>
--           <div class="radio">
--             <label class="radio-inli2ne">
--               ^{fvInput sexView}

--         <div .form-group>
--           <label .control-label>^{fvLabel dobView}
--           <div .input-group .date>
--             ^{fvInput dobView}
--             <span .input-group-addon>
--               <i .glyphicon .glyphicon-calendar>
--         <div .form-group>
--           <label .control-label>^{fvLabel pdgaView}
--           ^{fvInput pdgaView}
--         <div .form-group>
--           <label>^{fvLabel msView}
--           <div .radio>
--             ^{fvInput msView}
--         <div .form-group>
--           <label>^{fvLabel licenseView}
--           <div .radio>
--             ^{fvInput licenseView}
--         <div .form-group>
--           <label .c-input .c-checkbox>
--             ^{fvInput msDiscView}_{MsgMembershipDiscLabel}
--             <span .c-indicator>
--         <div .form-group>
--           <label .c-input .c-checkbox>
--             ^{fvInput magazineView}_{MsgMagazineLabel}
--             <span .c-indicator>
--         <div .form-group>
--           <label .control-label>^{fvLabel checkView}
--           ^{fvInput checkView}
--         <div .form-group>
--           <input type=submit .btn .btn-secondary .btn-block .btn-lg value=_{MsgSend}>
--       |]
--   return (result, widget)