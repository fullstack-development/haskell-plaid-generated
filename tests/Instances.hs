{-# OPTIONS_GHC -fno-warn-unused-imports -fno-warn-unused-matches #-}

module Instances where

import ThePlaid.Model
import ThePlaid.Core

import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as BL
import qualified Data.HashMap.Strict as HM
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Time as TI
import qualified Data.Vector as V

import Control.Monad
import Data.Char (isSpace)
import Data.List (sort)
import Test.QuickCheck

import ApproxEq

instance Arbitrary T.Text where
  arbitrary = T.pack <$> arbitrary

instance Arbitrary TI.Day where
  arbitrary = TI.ModifiedJulianDay . (2000 +) <$> arbitrary
  shrink = (TI.ModifiedJulianDay <$>) . shrink . TI.toModifiedJulianDay

instance Arbitrary TI.UTCTime where
  arbitrary =
    TI.UTCTime <$> arbitrary <*> (TI.secondsToDiffTime <$> choose (0, 86401))

instance Arbitrary BL.ByteString where
    arbitrary = BL.pack <$> arbitrary
    shrink xs = BL.pack <$> shrink (BL.unpack xs)

instance Arbitrary ByteArray where
    arbitrary = ByteArray <$> arbitrary
    shrink (ByteArray xs) = ByteArray <$> shrink xs

instance Arbitrary Binary where
    arbitrary = Binary <$> arbitrary
    shrink (Binary xs) = Binary <$> shrink xs

instance Arbitrary DateTime where
    arbitrary = DateTime <$> arbitrary
    shrink (DateTime xs) = DateTime <$> shrink xs

instance Arbitrary Date where
    arbitrary = Date <$> arbitrary
    shrink (Date xs) = Date <$> shrink xs

-- | A naive Arbitrary instance for A.Value:
-- instance Arbitrary A.Value where
--   arbitrary = frequency [(3, simpleTypes), (1, arrayTypes), (1, objectTypes)]
--     where
--       simpleTypes :: Gen A.Value
--       simpleTypes =
--         frequency
--           [ (1, return A.Null)
--           , (2, liftM A.Bool (arbitrary :: Gen Bool))
--           , (2, liftM (A.Number . fromIntegral) (arbitrary :: Gen Int))
--           , (2, liftM (A.String . T.pack) (arbitrary :: Gen String))
--           ]
--       mapF (k, v) = (T.pack k, v)
--       simpleAndArrays = frequency [(1, sized sizedArray), (4, simpleTypes)]
--       arrayTypes = sized sizedArray
--       objectTypes = sized sizedObject
--       sizedArray n = liftM (A.Array . V.fromList) $ replicateM n simpleTypes
--       sizedObject n =
--         liftM (A.object . map mapF) $
--         replicateM n $ (,) <$> (arbitrary :: Gen String) <*> simpleAndArrays
    
-- | Checks if a given list has no duplicates in _O(n log n)_.
hasNoDups
  :: (Ord a)
  => [a] -> Bool
hasNoDups = go Set.empty
  where
    go _ [] = True
    go s (x:xs)
      | s' <- Set.insert x s
      , Set.size s' > Set.size s = go s' xs
      | otherwise = False

instance ApproxEq TI.Day where
  (=~) = (==)
    
arbitraryReduced :: Arbitrary a => Int -> Gen a
arbitraryReduced n = resize (n `div` 2) arbitrary

arbitraryReducedMaybe :: Arbitrary a => Int -> Gen (Maybe a)
arbitraryReducedMaybe 0 = elements [Nothing]
arbitraryReducedMaybe n = arbitraryReduced n

arbitraryReducedMaybeValue :: Int -> Gen (Maybe A.Value)
arbitraryReducedMaybeValue 0 = elements [Nothing]
arbitraryReducedMaybeValue n = do
  generated <- arbitraryReduced n
  if generated == Just A.Null
    then return Nothing
    else return generated

-- * Models
 
instance Arbitrary APR where
  arbitrary = sized genAPR

genAPR :: Int -> Gen APR
genAPR n =
  APR
    <$> arbitrary -- aPRAprPercentage :: Double
    <*> arbitrary -- aPRAprType :: E'AprType
    <*> arbitraryReducedMaybe n -- aPRBalanceSubjectToApr :: Maybe Double
    <*> arbitraryReducedMaybe n -- aPRInterestChargeAmount :: Maybe Double
  
instance Arbitrary AccountAssets where
  arbitrary = sized genAccountAssets

genAccountAssets :: Int -> Gen AccountAssets
genAccountAssets n =
  AccountAssets
    <$> arbitrary -- accountAssetsAccountId :: Text
    <*> arbitraryReducedMaybe n -- accountAssetsPersistentAccountId :: Maybe Text
    <*> arbitraryReduced n -- accountAssetsBalances :: AccountBalance
    <*> arbitraryReducedMaybe n -- accountAssetsMask :: Maybe Text
    <*> arbitrary -- accountAssetsName :: Text
    <*> arbitraryReducedMaybe n -- accountAssetsOfficialName :: Maybe Text
    <*> arbitraryReduced n -- accountAssetsType :: AccountType
    <*> arbitraryReduced n -- accountAssetsSubtype :: AccountSubtype
    <*> arbitraryReducedMaybe n -- accountAssetsVerificationStatus :: Maybe E'VerificationStatus2
    <*> arbitraryReducedMaybe n -- accountAssetsDaysAvailable :: Maybe Double
    <*> arbitraryReducedMaybe n -- accountAssetsTransactions :: Maybe [AssetReportTransaction]
    <*> arbitraryReduced n -- accountAssetsOwners :: [Owner]
    <*> arbitraryReducedMaybe n -- accountAssetsHistoricalBalances :: Maybe [HistoricalBalance]
  
instance Arbitrary AccountAssetsAllOf where
  arbitrary = sized genAccountAssetsAllOf

genAccountAssetsAllOf :: Int -> Gen AccountAssetsAllOf
genAccountAssetsAllOf n =
  AccountAssetsAllOf
    <$> arbitraryReducedMaybe n -- accountAssetsAllOfDaysAvailable :: Maybe Double
    <*> arbitraryReducedMaybe n -- accountAssetsAllOfTransactions :: Maybe [AssetReportTransaction]
    <*> arbitraryReduced n -- accountAssetsAllOfOwners :: [Owner]
    <*> arbitraryReducedMaybe n -- accountAssetsAllOfHistoricalBalances :: Maybe [HistoricalBalance]
  
instance Arbitrary AccountBalance where
  arbitrary = sized genAccountBalance

genAccountBalance :: Int -> Gen AccountBalance
genAccountBalance n =
  AccountBalance
    <$> arbitraryReducedMaybe n -- accountBalanceAvailable :: Maybe Double
    <*> arbitrary -- accountBalanceCurrent :: Double
    <*> arbitraryReducedMaybe n -- accountBalanceLimit :: Maybe Double
    <*> arbitraryReducedMaybe n -- accountBalanceIsoCurrencyCode :: Maybe Text
    <*> arbitraryReducedMaybe n -- accountBalanceUnofficialCurrencyCode :: Maybe Text
  
instance Arbitrary AccountBase where
  arbitrary = sized genAccountBase

genAccountBase :: Int -> Gen AccountBase
genAccountBase n =
  AccountBase
    <$> arbitrary -- accountBaseAccountId :: Text
    <*> arbitraryReducedMaybe n -- accountBasePersistentAccountId :: Maybe Text
    <*> arbitraryReduced n -- accountBaseBalances :: AccountBalance
    <*> arbitraryReducedMaybe n -- accountBaseMask :: Maybe Text
    <*> arbitrary -- accountBaseName :: Text
    <*> arbitraryReducedMaybe n -- accountBaseOfficialName :: Maybe Text
    <*> arbitraryReduced n -- accountBaseType :: AccountType
    <*> arbitraryReduced n -- accountBaseSubtype :: AccountSubtype
    <*> arbitraryReducedMaybe n -- accountBaseVerificationStatus :: Maybe E'VerificationStatus2
  
instance Arbitrary AccountFiltersResponse where
  arbitrary = sized genAccountFiltersResponse

genAccountFiltersResponse :: Int -> Gen AccountFiltersResponse
genAccountFiltersResponse n =
  AccountFiltersResponse
    <$> arbitraryReducedMaybe n -- accountFiltersResponseDepository :: Maybe DepositoryFilter
    <*> arbitraryReducedMaybe n -- accountFiltersResponseCredit :: Maybe CreditFilter
    <*> arbitraryReducedMaybe n -- accountFiltersResponseLoan :: Maybe LoanFilter
    <*> arbitraryReducedMaybe n -- accountFiltersResponseInvestment :: Maybe InvestmentFilter
  
instance Arbitrary AccountIdentity where
  arbitrary = sized genAccountIdentity

genAccountIdentity :: Int -> Gen AccountIdentity
genAccountIdentity n =
  AccountIdentity
    <$> arbitrary -- accountIdentityAccountId :: Text
    <*> arbitraryReduced n -- accountIdentityBalances :: AccountBalance
    <*> arbitraryReducedMaybe n -- accountIdentityMask :: Maybe Text
    <*> arbitrary -- accountIdentityName :: Text
    <*> arbitraryReducedMaybe n -- accountIdentityOfficialName :: Maybe Text
    <*> arbitraryReduced n -- accountIdentityType :: AccountType
    <*> arbitraryReduced n -- accountIdentitySubtype :: AccountSubtype
    <*> arbitraryReducedMaybe n -- accountIdentityVerificationStatus :: Maybe E'VerificationStatus2
    <*> arbitraryReduced n -- accountIdentityOwners :: [Owner]
  
instance Arbitrary AccountIdentityAllOf where
  arbitrary = sized genAccountIdentityAllOf

genAccountIdentityAllOf :: Int -> Gen AccountIdentityAllOf
genAccountIdentityAllOf n =
  AccountIdentityAllOf
    <$> arbitraryReduced n -- accountIdentityAllOfOwners :: [Owner]
  
instance Arbitrary AccessToken where
  arbitrary = AccessToken <$> arbitrary
  
instance Arbitrary AccountsBalanceGetRequest where
  arbitrary = sized genAccountsBalanceGetRequest

genAccountsBalanceGetRequest :: Int -> Gen AccountsBalanceGetRequest
genAccountsBalanceGetRequest n =
  AccountsBalanceGetRequest
    <$> arbitrary -- accountsBalanceGetRequestAccessToken :: AccessToken
    <*> arbitraryReducedMaybe n -- accountsBalanceGetRequestSecret :: Maybe Text
    <*> arbitraryReducedMaybe n -- accountsBalanceGetRequestClientId :: Maybe Text
    <*> arbitraryReducedMaybe n -- accountsBalanceGetRequestOptions :: Maybe AccountsBalanceGetRequestOptions
  
instance Arbitrary AccountsBalanceGetRequestOptions where
  arbitrary = sized genAccountsBalanceGetRequestOptions

genAccountsBalanceGetRequestOptions :: Int -> Gen AccountsBalanceGetRequestOptions
genAccountsBalanceGetRequestOptions n =
  AccountsBalanceGetRequestOptions
    <$> arbitraryReducedMaybe n -- accountsBalanceGetRequestOptionsAccountIds :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- accountsBalanceGetRequestOptionsMinLastUpdatedDatetime :: Maybe UTCTime
  
instance Arbitrary AccountsGetRequest where
  arbitrary = sized genAccountsGetRequest

genAccountsGetRequest :: Int -> Gen AccountsGetRequest
genAccountsGetRequest n =
  AccountsGetRequest
    <$> arbitraryReducedMaybe n -- accountsGetRequestClientId :: Maybe Text
    <*> arbitraryReducedMaybe n -- accountsGetRequestSecret :: Maybe Text
    <*> arbitrary -- accountsGetRequestAccessToken :: Text
    <*> arbitraryReducedMaybe n -- accountsGetRequestOptions :: Maybe AccountsGetRequestOptions
  
instance Arbitrary AccountsGetRequestOptions where
  arbitrary = sized genAccountsGetRequestOptions

genAccountsGetRequestOptions :: Int -> Gen AccountsGetRequestOptions
genAccountsGetRequestOptions n =
  AccountsGetRequestOptions
    <$> arbitraryReducedMaybe n -- accountsGetRequestOptionsAccountIds :: Maybe [Text]
  
instance Arbitrary AccountsGetResponse where
  arbitrary = sized genAccountsGetResponse

genAccountsGetResponse :: Int -> Gen AccountsGetResponse
genAccountsGetResponse n =
  AccountsGetResponse
    <$> arbitraryReduced n -- accountsGetResponseAccounts :: [AccountBase]
    <*> arbitraryReduced n -- accountsGetResponseItem :: Item
    <*> arbitrary -- accountsGetResponseRequestId :: Text
  
instance Arbitrary Address where
  arbitrary = sized genAddress

genAddress :: Int -> Gen Address
genAddress n =
  Address
    <$> arbitraryReduced n -- addressData :: AddressData
    <*> arbitraryReducedMaybe n -- addressPrimary :: Maybe Bool
  
instance Arbitrary AddressData where
  arbitrary = sized genAddressData

genAddressData :: Int -> Gen AddressData
genAddressData n =
  AddressData
    <$> arbitrary -- addressDataCity :: Text
    <*> arbitraryReducedMaybe n -- addressDataRegion :: Maybe Text
    <*> arbitrary -- addressDataStreet :: Text
    <*> arbitraryReducedMaybe n -- addressDataPostalCode :: Maybe Text
    <*> arbitrary -- addressDataCountry :: Text
  
instance Arbitrary Amount where
  arbitrary = sized genAmount

genAmount :: Int -> Gen Amount
genAmount n =
  Amount
    <$> arbitrary -- amountCurrency :: E'Currency
    <*> arbitrary -- amountValue :: Double
  
instance Arbitrary AssetReport where
  arbitrary = sized genAssetReport

genAssetReport :: Int -> Gen AssetReport
genAssetReport n =
  AssetReport
    <$> arbitrary -- assetReportAssetReportId :: Text
    <*> arbitrary -- assetReportClientReportId :: Text
    <*> arbitrary -- assetReportDateGenerated :: Text
    <*> arbitrary -- assetReportDaysRequested :: Double
    <*> arbitraryReduced n -- assetReportUser :: AssetReportUser
    <*> arbitraryReduced n -- assetReportItems :: [AssetReportItem]
  
instance Arbitrary AssetReportAuditCopyCreateRequest where
  arbitrary = sized genAssetReportAuditCopyCreateRequest

genAssetReportAuditCopyCreateRequest :: Int -> Gen AssetReportAuditCopyCreateRequest
genAssetReportAuditCopyCreateRequest n =
  AssetReportAuditCopyCreateRequest
    <$> arbitraryReducedMaybe n -- assetReportAuditCopyCreateRequestClientId :: Maybe Text
    <*> arbitraryReducedMaybe n -- assetReportAuditCopyCreateRequestSecret :: Maybe Text
    <*> arbitrary -- assetReportAuditCopyCreateRequestAssetReportToken :: Text
    <*> arbitrary -- assetReportAuditCopyCreateRequestAuditorId :: Text
  
instance Arbitrary AssetReportAuditCopyCreateResponse where
  arbitrary = sized genAssetReportAuditCopyCreateResponse

genAssetReportAuditCopyCreateResponse :: Int -> Gen AssetReportAuditCopyCreateResponse
genAssetReportAuditCopyCreateResponse n =
  AssetReportAuditCopyCreateResponse
    <$> arbitrary -- assetReportAuditCopyCreateResponseAuditCopyToken :: Text
    <*> arbitrary -- assetReportAuditCopyCreateResponseRequestId :: Text
  
instance Arbitrary AssetReportAuditCopyGetRequest where
  arbitrary = sized genAssetReportAuditCopyGetRequest

genAssetReportAuditCopyGetRequest :: Int -> Gen AssetReportAuditCopyGetRequest
genAssetReportAuditCopyGetRequest n =
  AssetReportAuditCopyGetRequest
    <$> arbitraryReducedMaybe n -- assetReportAuditCopyGetRequestClientId :: Maybe Text
    <*> arbitraryReducedMaybe n -- assetReportAuditCopyGetRequestSecret :: Maybe Text
    <*> arbitrary -- assetReportAuditCopyGetRequestAuditCopyToken :: Text
  
instance Arbitrary AssetReportAuditCopyRemoveRequest where
  arbitrary = sized genAssetReportAuditCopyRemoveRequest

genAssetReportAuditCopyRemoveRequest :: Int -> Gen AssetReportAuditCopyRemoveRequest
genAssetReportAuditCopyRemoveRequest n =
  AssetReportAuditCopyRemoveRequest
    <$> arbitraryReducedMaybe n -- assetReportAuditCopyRemoveRequestClientId :: Maybe Text
    <*> arbitraryReducedMaybe n -- assetReportAuditCopyRemoveRequestSecret :: Maybe Text
    <*> arbitrary -- assetReportAuditCopyRemoveRequestAuditCopyToken :: Text
  
instance Arbitrary AssetReportAuditCopyRemoveResponse where
  arbitrary = sized genAssetReportAuditCopyRemoveResponse

genAssetReportAuditCopyRemoveResponse :: Int -> Gen AssetReportAuditCopyRemoveResponse
genAssetReportAuditCopyRemoveResponse n =
  AssetReportAuditCopyRemoveResponse
    <$> arbitrary -- assetReportAuditCopyRemoveResponseRemoved :: Bool
    <*> arbitrary -- assetReportAuditCopyRemoveResponseRequestId :: Text
  
instance Arbitrary AssetReportCreateRequest where
  arbitrary = sized genAssetReportCreateRequest

genAssetReportCreateRequest :: Int -> Gen AssetReportCreateRequest
genAssetReportCreateRequest n =
  AssetReportCreateRequest
    <$> arbitraryReducedMaybe n -- assetReportCreateRequestClientId :: Maybe Text
    <*> arbitraryReducedMaybe n -- assetReportCreateRequestSecret :: Maybe Text
    <*> arbitrary -- assetReportCreateRequestAccessTokens :: [Text]
    <*> arbitrary -- assetReportCreateRequestDaysRequested :: Int
    <*> arbitraryReducedMaybe n -- assetReportCreateRequestOptions :: Maybe AssetReportCreateRequestOptions
  
instance Arbitrary AssetReportCreateRequestOptions where
  arbitrary = sized genAssetReportCreateRequestOptions

genAssetReportCreateRequestOptions :: Int -> Gen AssetReportCreateRequestOptions
genAssetReportCreateRequestOptions n =
  AssetReportCreateRequestOptions
    <$> arbitraryReducedMaybe n -- assetReportCreateRequestOptionsClientReportId :: Maybe Text
    <*> arbitraryReducedMaybe n -- assetReportCreateRequestOptionsWebhook :: Maybe Text
    <*> arbitraryReducedMaybe n -- assetReportCreateRequestOptionsUser :: Maybe AssetReportUser
  
instance Arbitrary AssetReportCreateResponse where
  arbitrary = sized genAssetReportCreateResponse

genAssetReportCreateResponse :: Int -> Gen AssetReportCreateResponse
genAssetReportCreateResponse n =
  AssetReportCreateResponse
    <$> arbitrary -- assetReportCreateResponseAssetReportToken :: Text
    <*> arbitrary -- assetReportCreateResponseAssetReportId :: Text
    <*> arbitrary -- assetReportCreateResponseRequestId :: Text
  
instance Arbitrary AssetReportFilterRequest where
  arbitrary = sized genAssetReportFilterRequest

genAssetReportFilterRequest :: Int -> Gen AssetReportFilterRequest
genAssetReportFilterRequest n =
  AssetReportFilterRequest
    <$> arbitraryReducedMaybe n -- assetReportFilterRequestClientId :: Maybe Text
    <*> arbitraryReducedMaybe n -- assetReportFilterRequestSecret :: Maybe Text
    <*> arbitrary -- assetReportFilterRequestAssetReportToken :: Text
    <*> arbitrary -- assetReportFilterRequestAccountIdsToExclude :: [Text]
  
instance Arbitrary AssetReportFilterResponse where
  arbitrary = sized genAssetReportFilterResponse

genAssetReportFilterResponse :: Int -> Gen AssetReportFilterResponse
genAssetReportFilterResponse n =
  AssetReportFilterResponse
    <$> arbitrary -- assetReportFilterResponseAssetReportToken :: Text
    <*> arbitrary -- assetReportFilterResponseAssetReportId :: Text
    <*> arbitrary -- assetReportFilterResponseRequestId :: Text
  
instance Arbitrary AssetReportGetRequest where
  arbitrary = sized genAssetReportGetRequest

genAssetReportGetRequest :: Int -> Gen AssetReportGetRequest
genAssetReportGetRequest n =
  AssetReportGetRequest
    <$> arbitraryReducedMaybe n -- assetReportGetRequestClientId :: Maybe Text
    <*> arbitraryReducedMaybe n -- assetReportGetRequestSecret :: Maybe Text
    <*> arbitrary -- assetReportGetRequestAssetReportToken :: Text
    <*> arbitraryReducedMaybe n -- assetReportGetRequestIncludeInsights :: Maybe Bool
  
instance Arbitrary AssetReportGetResponse where
  arbitrary = sized genAssetReportGetResponse

genAssetReportGetResponse :: Int -> Gen AssetReportGetResponse
genAssetReportGetResponse n =
  AssetReportGetResponse
    <$> arbitraryReduced n -- assetReportGetResponseReport :: AssetReport
    <*> arbitraryReduced n -- assetReportGetResponseWarnings :: [Warning]
    <*> arbitrary -- assetReportGetResponseRequestId :: Text
  
instance Arbitrary AssetReportItem where
  arbitrary = sized genAssetReportItem

genAssetReportItem :: Int -> Gen AssetReportItem
genAssetReportItem n =
  AssetReportItem
    <$> arbitrary -- assetReportItemItemId :: Text
    <*> arbitrary -- assetReportItemInstitutionName :: Text
    <*> arbitrary -- assetReportItemInstitutionId :: Text
    <*> arbitrary -- assetReportItemDateLastUpdated :: Text
    <*> arbitraryReduced n -- assetReportItemAccounts :: [AccountAssets]
  
instance Arbitrary AssetReportPDFGetRequest where
  arbitrary = sized genAssetReportPDFGetRequest

genAssetReportPDFGetRequest :: Int -> Gen AssetReportPDFGetRequest
genAssetReportPDFGetRequest n =
  AssetReportPDFGetRequest
    <$> arbitraryReducedMaybe n -- assetReportPDFGetRequestClientId :: Maybe Text
    <*> arbitraryReducedMaybe n -- assetReportPDFGetRequestSecret :: Maybe Text
    <*> arbitrary -- assetReportPDFGetRequestAssetReportToken :: Text
  
instance Arbitrary AssetReportRefreshRequest where
  arbitrary = sized genAssetReportRefreshRequest

genAssetReportRefreshRequest :: Int -> Gen AssetReportRefreshRequest
genAssetReportRefreshRequest n =
  AssetReportRefreshRequest
    <$> arbitraryReducedMaybe n -- assetReportRefreshRequestClientId :: Maybe Text
    <*> arbitraryReducedMaybe n -- assetReportRefreshRequestSecret :: Maybe Text
    <*> arbitrary -- assetReportRefreshRequestAssetReportToken :: Text
    <*> arbitraryReducedMaybe n -- assetReportRefreshRequestDaysRequested :: Maybe Int
    <*> arbitraryReducedMaybe n -- assetReportRefreshRequestOptions :: Maybe AssetReportRefreshRequestOptions
  
instance Arbitrary AssetReportRefreshRequestOptions where
  arbitrary = sized genAssetReportRefreshRequestOptions

genAssetReportRefreshRequestOptions :: Int -> Gen AssetReportRefreshRequestOptions
genAssetReportRefreshRequestOptions n =
  AssetReportRefreshRequestOptions
    <$> arbitraryReducedMaybe n -- assetReportRefreshRequestOptionsClientReportId :: Maybe Text
    <*> arbitraryReducedMaybe n -- assetReportRefreshRequestOptionsWebhook :: Maybe Text
    <*> arbitraryReducedMaybe n -- assetReportRefreshRequestOptionsUser :: Maybe AssetReportUser
  
instance Arbitrary AssetReportRefreshResponse where
  arbitrary = sized genAssetReportRefreshResponse

genAssetReportRefreshResponse :: Int -> Gen AssetReportRefreshResponse
genAssetReportRefreshResponse n =
  AssetReportRefreshResponse
    <$> arbitrary -- assetReportRefreshResponseAssetReportId :: Text
    <*> arbitrary -- assetReportRefreshResponseAssetReportToken :: Text
    <*> arbitrary -- assetReportRefreshResponseRequestId :: Text
  
instance Arbitrary AssetReportRemoveRequest where
  arbitrary = sized genAssetReportRemoveRequest

genAssetReportRemoveRequest :: Int -> Gen AssetReportRemoveRequest
genAssetReportRemoveRequest n =
  AssetReportRemoveRequest
    <$> arbitraryReducedMaybe n -- assetReportRemoveRequestClientId :: Maybe Text
    <*> arbitraryReducedMaybe n -- assetReportRemoveRequestSecret :: Maybe Text
    <*> arbitrary -- assetReportRemoveRequestAssetReportToken :: Text
  
instance Arbitrary AssetReportRemoveResponse where
  arbitrary = sized genAssetReportRemoveResponse

genAssetReportRemoveResponse :: Int -> Gen AssetReportRemoveResponse
genAssetReportRemoveResponse n =
  AssetReportRemoveResponse
    <$> arbitrary -- assetReportRemoveResponseRemoved :: Bool
    <*> arbitrary -- assetReportRemoveResponseRequestId :: Text
  
instance Arbitrary AssetReportTransaction where
  arbitrary = sized genAssetReportTransaction

genAssetReportTransaction :: Int -> Gen AssetReportTransaction
genAssetReportTransaction n =
  AssetReportTransaction
    <$> arbitraryReducedMaybe n -- assetReportTransactionTransactionType :: Maybe E'TransactionType
    <*> arbitrary -- assetReportTransactionTransactionId :: Text
    <*> arbitraryReducedMaybe n -- assetReportTransactionAccountOwner :: Maybe Text
    <*> arbitraryReducedMaybe n -- assetReportTransactionPendingTransactionId :: Maybe Text
    <*> arbitrary -- assetReportTransactionPending :: Bool
    <*> arbitraryReducedMaybe n -- assetReportTransactionPaymentChannel :: Maybe E'PaymentChannel
    <*> arbitraryReducedMaybe n -- assetReportTransactionPaymentMeta :: Maybe PaymentMeta
    <*> arbitraryReducedMaybe n -- assetReportTransactionName :: Maybe Text
    <*> arbitraryReducedMaybe n -- assetReportTransactionMerchantName :: Maybe Text
    <*> arbitraryReducedMaybe n -- assetReportTransactionLocation :: Maybe Location
    <*> arbitraryReducedMaybe n -- assetReportTransactionAuthorizedDate :: Maybe Text
    <*> arbitraryReducedMaybe n -- assetReportTransactionAuthorizedDatetime :: Maybe Text
    <*> arbitrary -- assetReportTransactionDate :: Text
    <*> arbitraryReducedMaybe n -- assetReportTransactionDatetime :: Maybe Text
    <*> arbitraryReducedMaybe n -- assetReportTransactionCategoryId :: Maybe Text
    <*> arbitraryReducedMaybe n -- assetReportTransactionCategory :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- assetReportTransactionUnofficialCurrencyCode :: Maybe Text
    <*> arbitraryReducedMaybe n -- assetReportTransactionIsoCurrencyCode :: Maybe Text
    <*> arbitrary -- assetReportTransactionAmount :: Double
    <*> arbitrary -- assetReportTransactionAccountId :: Text
    <*> arbitraryReducedMaybe n -- assetReportTransactionTransactionCode :: Maybe TransactionCode
    <*> arbitraryReducedMaybe n -- assetReportTransactionDateTransacted :: Maybe Text
    <*> arbitrary -- assetReportTransactionOriginalDescription :: Text
  
instance Arbitrary AssetReportTransactionAllOf where
  arbitrary = sized genAssetReportTransactionAllOf

genAssetReportTransactionAllOf :: Int -> Gen AssetReportTransactionAllOf
genAssetReportTransactionAllOf n =
  AssetReportTransactionAllOf
    <$> arbitraryReducedMaybe n -- assetReportTransactionAllOfDateTransacted :: Maybe Text
    <*> arbitrary -- assetReportTransactionAllOfOriginalDescription :: Text
  
instance Arbitrary AssetReportUser where
  arbitrary = sized genAssetReportUser

genAssetReportUser :: Int -> Gen AssetReportUser
genAssetReportUser n =
  AssetReportUser
    <$> arbitraryReducedMaybe n -- assetReportUserClientUserId :: Maybe Text
    <*> arbitraryReducedMaybe n -- assetReportUserFirstName :: Maybe Text
    <*> arbitraryReducedMaybe n -- assetReportUserMiddleName :: Maybe Text
    <*> arbitraryReducedMaybe n -- assetReportUserLastName :: Maybe Text
    <*> arbitraryReducedMaybe n -- assetReportUserSsn :: Maybe Text
    <*> arbitraryReducedMaybe n -- assetReportUserPhoneNumber :: Maybe Text
    <*> arbitraryReducedMaybe n -- assetReportUserEmail :: Maybe Text
  
instance Arbitrary AssetsErrorWebhook where
  arbitrary = sized genAssetsErrorWebhook

genAssetsErrorWebhook :: Int -> Gen AssetsErrorWebhook
genAssetsErrorWebhook n =
  AssetsErrorWebhook
    <$> arbitrary -- assetsErrorWebhookWebhookType :: Text
    <*> arbitrary -- assetsErrorWebhookWebhookCode :: Text
    <*> arbitraryReduced n -- assetsErrorWebhookError :: Error
    <*> arbitrary -- assetsErrorWebhookAssetReportId :: Text
  
instance Arbitrary AssetsProductReadyWebhook where
  arbitrary = sized genAssetsProductReadyWebhook

genAssetsProductReadyWebhook :: Int -> Gen AssetsProductReadyWebhook
genAssetsProductReadyWebhook n =
  AssetsProductReadyWebhook
    <$> arbitrary -- assetsProductReadyWebhookWebhookType :: Text
    <*> arbitrary -- assetsProductReadyWebhookWebhookCode :: Text
    <*> arbitrary -- assetsProductReadyWebhookAssetReportId :: Text
  
instance Arbitrary AuthGetNumbers where
  arbitrary = sized genAuthGetNumbers

genAuthGetNumbers :: Int -> Gen AuthGetNumbers
genAuthGetNumbers n =
  AuthGetNumbers
    <$> arbitraryReducedMaybe n -- authGetNumbersAch :: Maybe [NumbersACH]
    <*> arbitraryReducedMaybe n -- authGetNumbersEft :: Maybe [NumbersEFT]
    <*> arbitraryReducedMaybe n -- authGetNumbersInternational :: Maybe [NumbersInternationals]
    <*> arbitraryReducedMaybe n -- authGetNumbersBacs :: Maybe [NumbersBACS]
  
instance Arbitrary AuthGetRequest where
  arbitrary = sized genAuthGetRequest

genAuthGetRequest :: Int -> Gen AuthGetRequest
genAuthGetRequest n =
  AuthGetRequest
    <$> arbitraryReducedMaybe n -- authGetRequestClientId :: Maybe Text
    <*> arbitraryReducedMaybe n -- authGetRequestSecret :: Maybe Text
    <*> arbitrary -- authGetRequestAccessToken :: Text
    <*> arbitraryReducedMaybe n -- authGetRequestOptions :: Maybe AuthGetRequestOptions
  
instance Arbitrary AuthGetRequestOptions where
  arbitrary = sized genAuthGetRequestOptions

genAuthGetRequestOptions :: Int -> Gen AuthGetRequestOptions
genAuthGetRequestOptions n =
  AuthGetRequestOptions
    <$> arbitraryReducedMaybe n -- authGetRequestOptionsAccountIds :: Maybe [Text]
  
instance Arbitrary AuthGetResponse where
  arbitrary = sized genAuthGetResponse

genAuthGetResponse :: Int -> Gen AuthGetResponse
genAuthGetResponse n =
  AuthGetResponse
    <$> arbitraryReduced n -- authGetResponseAccounts :: [AccountBase]
    <*> arbitraryReduced n -- authGetResponseNumbers :: AuthGetNumbers
    <*> arbitraryReduced n -- authGetResponseItem :: Item
    <*> arbitrary -- authGetResponseRequestId :: Text
  
instance Arbitrary AutomaticallyVerifiedWebhook where
  arbitrary = sized genAutomaticallyVerifiedWebhook

genAutomaticallyVerifiedWebhook :: Int -> Gen AutomaticallyVerifiedWebhook
genAutomaticallyVerifiedWebhook n =
  AutomaticallyVerifiedWebhook
    <$> arbitrary -- automaticallyVerifiedWebhookWebhookType :: Text
    <*> arbitrary -- automaticallyVerifiedWebhookWebhookCode :: Text
    <*> arbitrary -- automaticallyVerifiedWebhookAccountId :: Text
    <*> arbitrary -- automaticallyVerifiedWebhookItemId :: Text
  
instance Arbitrary BankTransfer where
  arbitrary = sized genBankTransfer

genBankTransfer :: Int -> Gen BankTransfer
genBankTransfer n =
  BankTransfer
    <$> arbitrary -- bankTransferId :: Text
    <*> arbitraryReduced n -- bankTransferAchClass :: ACHClass
    <*> arbitrary -- bankTransferAccountId :: Text
    <*> arbitraryReduced n -- bankTransferType :: BankTransferType
    <*> arbitraryReduced n -- bankTransferUser :: BankTransferUser
    <*> arbitrary -- bankTransferAmount :: Text
    <*> arbitrary -- bankTransferIsoCurrencyCode :: Text
    <*> arbitrary -- bankTransferDescription :: Text
    <*> arbitrary -- bankTransferCreated :: Text
    <*> arbitraryReduced n -- bankTransferStatus :: BankTransferStatus
    <*> arbitraryReduced n -- bankTransferNetwork :: BankTransferNetwork
    <*> arbitrary -- bankTransferCancellable :: Bool
    <*> arbitraryReducedMaybe n -- bankTransferFailureReason :: Maybe BankTransferFailure
    <*> arbitraryReducedMaybe n -- bankTransferCustomTag :: Maybe Text
    <*> arbitrary -- bankTransferMetadata :: (Map.Map String Text)
    <*> arbitrary -- bankTransferOriginationAccountId :: Text
    <*> arbitraryReduced n -- bankTransferDirection :: BankTransferDirection
  
instance Arbitrary BankTransferBalance where
  arbitrary = sized genBankTransferBalance

genBankTransferBalance :: Int -> Gen BankTransferBalance
genBankTransferBalance n =
  BankTransferBalance
    <$> arbitrary -- bankTransferBalanceAvailable :: Text
    <*> arbitrary -- bankTransferBalanceTransactable :: Text
  
instance Arbitrary BankTransferBalanceGetRequest where
  arbitrary = sized genBankTransferBalanceGetRequest

genBankTransferBalanceGetRequest :: Int -> Gen BankTransferBalanceGetRequest
genBankTransferBalanceGetRequest n =
  BankTransferBalanceGetRequest
    <$> arbitraryReducedMaybe n -- bankTransferBalanceGetRequestClientId :: Maybe Text
    <*> arbitraryReducedMaybe n -- bankTransferBalanceGetRequestSecret :: Maybe Text
    <*> arbitraryReducedMaybe n -- bankTransferBalanceGetRequestOriginationAccountId :: Maybe Text
  
instance Arbitrary BankTransferBalanceGetResponse where
  arbitrary = sized genBankTransferBalanceGetResponse

genBankTransferBalanceGetResponse :: Int -> Gen BankTransferBalanceGetResponse
genBankTransferBalanceGetResponse n =
  BankTransferBalanceGetResponse
    <$> arbitraryReduced n -- bankTransferBalanceGetResponseBalance :: BankTransferBalance
    <*> arbitrary -- bankTransferBalanceGetResponseOriginationAccountId :: Text
    <*> arbitrary -- bankTransferBalanceGetResponseRequestId :: Text
  
instance Arbitrary BankTransferCancelRequest where
  arbitrary = sized genBankTransferCancelRequest

genBankTransferCancelRequest :: Int -> Gen BankTransferCancelRequest
genBankTransferCancelRequest n =
  BankTransferCancelRequest
    <$> arbitraryReducedMaybe n -- bankTransferCancelRequestClientId :: Maybe Text
    <*> arbitraryReducedMaybe n -- bankTransferCancelRequestSecret :: Maybe Text
    <*> arbitrary -- bankTransferCancelRequestBankTransferId :: Text
  
instance Arbitrary BankTransferCancelResponse where
  arbitrary = sized genBankTransferCancelResponse

genBankTransferCancelResponse :: Int -> Gen BankTransferCancelResponse
genBankTransferCancelResponse n =
  BankTransferCancelResponse
    <$> arbitrary -- bankTransferCancelResponseRequestId :: Text
  
instance Arbitrary BankTransferCreateRequest where
  arbitrary = sized genBankTransferCreateRequest

genBankTransferCreateRequest :: Int -> Gen BankTransferCreateRequest
genBankTransferCreateRequest n =
  BankTransferCreateRequest
    <$> arbitraryReducedMaybe n -- bankTransferCreateRequestClientId :: Maybe Text
    <*> arbitraryReducedMaybe n -- bankTransferCreateRequestSecret :: Maybe Text
    <*> arbitrary -- bankTransferCreateRequestIdempotencyKey :: Text
    <*> arbitrary -- bankTransferCreateRequestAccessToken :: Text
    <*> arbitrary -- bankTransferCreateRequestAccountId :: Text
    <*> arbitraryReduced n -- bankTransferCreateRequestType :: BankTransferType
    <*> arbitraryReduced n -- bankTransferCreateRequestNetwork :: BankTransferNetwork
    <*> arbitrary -- bankTransferCreateRequestAmount :: Text
    <*> arbitrary -- bankTransferCreateRequestIsoCurrencyCode :: Text
    <*> arbitrary -- bankTransferCreateRequestDescription :: Text
    <*> arbitraryReducedMaybe n -- bankTransferCreateRequestAchClass :: Maybe ACHClass
    <*> arbitraryReduced n -- bankTransferCreateRequestUser :: BankTransferUser
    <*> arbitraryReducedMaybe n -- bankTransferCreateRequestCustomTag :: Maybe Text
    <*> arbitraryReducedMaybe n -- bankTransferCreateRequestMetadata :: Maybe (Map.Map String Text)
    <*> arbitraryReducedMaybe n -- bankTransferCreateRequestOriginationAccountId :: Maybe Text
  
instance Arbitrary BankTransferCreateResponse where
  arbitrary = sized genBankTransferCreateResponse

genBankTransferCreateResponse :: Int -> Gen BankTransferCreateResponse
genBankTransferCreateResponse n =
  BankTransferCreateResponse
    <$> arbitraryReduced n -- bankTransferCreateResponseBankTransfer :: BankTransfer
    <*> arbitrary -- bankTransferCreateResponseRequestId :: Text
  
instance Arbitrary BankTransferEvent where
  arbitrary = sized genBankTransferEvent

genBankTransferEvent :: Int -> Gen BankTransferEvent
genBankTransferEvent n =
  BankTransferEvent
    <$> arbitrary -- bankTransferEventEventId :: Int
    <*> arbitrary -- bankTransferEventTimestamp :: Text
    <*> arbitraryReduced n -- bankTransferEventEventType :: BankTransferEventType
    <*> arbitrary -- bankTransferEventAccountId :: Text
    <*> arbitrary -- bankTransferEventBankTransferId :: Text
    <*> arbitraryReducedMaybe n -- bankTransferEventOriginationAccountId :: Maybe Text
    <*> arbitraryReduced n -- bankTransferEventBankTransferType :: BankTransferType
    <*> arbitrary -- bankTransferEventBankTransferAmount :: Text
    <*> arbitrary -- bankTransferEventBankTransferIsoCurrencyCode :: Text
    <*> arbitraryReduced n -- bankTransferEventFailureReason :: BankTransferFailure
    <*> arbitraryReduced n -- bankTransferEventDirection :: BankTransferDirection
    <*> arbitraryReduced n -- bankTransferEventReceiverDetails :: BankTransferReceiverDetails
  
instance Arbitrary BankTransferEventListRequest where
  arbitrary = sized genBankTransferEventListRequest

genBankTransferEventListRequest :: Int -> Gen BankTransferEventListRequest
genBankTransferEventListRequest n =
  BankTransferEventListRequest
    <$> arbitraryReducedMaybe n -- bankTransferEventListRequestClientId :: Maybe Text
    <*> arbitraryReducedMaybe n -- bankTransferEventListRequestSecret :: Maybe Text
    <*> arbitraryReducedMaybe n -- bankTransferEventListRequestStartDate :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- bankTransferEventListRequestEndDate :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- bankTransferEventListRequestBankTransferId :: Maybe Text
    <*> arbitraryReducedMaybe n -- bankTransferEventListRequestAccountId :: Maybe Text
    <*> arbitraryReducedMaybe n -- bankTransferEventListRequestBankTransferType :: Maybe E'BankTransferType
    <*> arbitraryReducedMaybe n -- bankTransferEventListRequestEventTypes :: Maybe [BankTransferEventType]
    <*> arbitraryReducedMaybe n -- bankTransferEventListRequestCount :: Maybe Int
    <*> arbitraryReducedMaybe n -- bankTransferEventListRequestOffset :: Maybe Int
    <*> arbitraryReducedMaybe n -- bankTransferEventListRequestOriginationAccountId :: Maybe Text
    <*> arbitraryReducedMaybe n -- bankTransferEventListRequestDirection :: Maybe Text
  
instance Arbitrary BankTransferEventListResponse where
  arbitrary = sized genBankTransferEventListResponse

genBankTransferEventListResponse :: Int -> Gen BankTransferEventListResponse
genBankTransferEventListResponse n =
  BankTransferEventListResponse
    <$> arbitraryReduced n -- bankTransferEventListResponseBankTransferEvents :: [BankTransferEvent]
    <*> arbitrary -- bankTransferEventListResponseRequestId :: Text
  
instance Arbitrary BankTransferEventSyncRequest where
  arbitrary = sized genBankTransferEventSyncRequest

genBankTransferEventSyncRequest :: Int -> Gen BankTransferEventSyncRequest
genBankTransferEventSyncRequest n =
  BankTransferEventSyncRequest
    <$> arbitraryReducedMaybe n -- bankTransferEventSyncRequestClientId :: Maybe Text
    <*> arbitraryReducedMaybe n -- bankTransferEventSyncRequestSecret :: Maybe Text
    <*> arbitrary -- bankTransferEventSyncRequestAfterId :: Int
    <*> arbitraryReducedMaybe n -- bankTransferEventSyncRequestCount :: Maybe Int
  
instance Arbitrary BankTransferEventSyncResponse where
  arbitrary = sized genBankTransferEventSyncResponse

genBankTransferEventSyncResponse :: Int -> Gen BankTransferEventSyncResponse
genBankTransferEventSyncResponse n =
  BankTransferEventSyncResponse
    <$> arbitraryReduced n -- bankTransferEventSyncResponseBankTransferEvents :: [BankTransferEvent]
    <*> arbitrary -- bankTransferEventSyncResponseRequestId :: Text
  
instance Arbitrary BankTransferFailure where
  arbitrary = sized genBankTransferFailure

genBankTransferFailure :: Int -> Gen BankTransferFailure
genBankTransferFailure n =
  BankTransferFailure
    <$> arbitraryReducedMaybe n -- bankTransferFailureAchReturnCode :: Maybe Text
    <*> arbitraryReducedMaybe n -- bankTransferFailureDescription :: Maybe Text
  
instance Arbitrary BankTransferGetRequest where
  arbitrary = sized genBankTransferGetRequest

genBankTransferGetRequest :: Int -> Gen BankTransferGetRequest
genBankTransferGetRequest n =
  BankTransferGetRequest
    <$> arbitraryReducedMaybe n -- bankTransferGetRequestClientId :: Maybe Text
    <*> arbitraryReducedMaybe n -- bankTransferGetRequestSecret :: Maybe Text
    <*> arbitrary -- bankTransferGetRequestBankTransferId :: Text
  
instance Arbitrary BankTransferGetResponse where
  arbitrary = sized genBankTransferGetResponse

genBankTransferGetResponse :: Int -> Gen BankTransferGetResponse
genBankTransferGetResponse n =
  BankTransferGetResponse
    <$> arbitraryReduced n -- bankTransferGetResponseBankTransfer :: BankTransfer
    <*> arbitrary -- bankTransferGetResponseRequestId :: Text
  
instance Arbitrary BankTransferListRequest where
  arbitrary = sized genBankTransferListRequest

genBankTransferListRequest :: Int -> Gen BankTransferListRequest
genBankTransferListRequest n =
  BankTransferListRequest
    <$> arbitraryReducedMaybe n -- bankTransferListRequestClientId :: Maybe Text
    <*> arbitraryReducedMaybe n -- bankTransferListRequestSecret :: Maybe Text
    <*> arbitraryReducedMaybe n -- bankTransferListRequestStartDate :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- bankTransferListRequestEndDate :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- bankTransferListRequestCount :: Maybe Int
    <*> arbitraryReducedMaybe n -- bankTransferListRequestOffset :: Maybe Int
    <*> arbitraryReducedMaybe n -- bankTransferListRequestOriginationAccountId :: Maybe Text
    <*> arbitraryReducedMaybe n -- bankTransferListRequestDirection :: Maybe BankTransferDirection
  
instance Arbitrary BankTransferListResponse where
  arbitrary = sized genBankTransferListResponse

genBankTransferListResponse :: Int -> Gen BankTransferListResponse
genBankTransferListResponse n =
  BankTransferListResponse
    <$> arbitraryReduced n -- bankTransferListResponseBankTransfers :: [BankTransfer]
    <*> arbitrary -- bankTransferListResponseRequestId :: Text
  
instance Arbitrary BankTransferMigrateAccountRequest where
  arbitrary = sized genBankTransferMigrateAccountRequest

genBankTransferMigrateAccountRequest :: Int -> Gen BankTransferMigrateAccountRequest
genBankTransferMigrateAccountRequest n =
  BankTransferMigrateAccountRequest
    <$> arbitraryReducedMaybe n -- bankTransferMigrateAccountRequestClientId :: Maybe Text
    <*> arbitraryReducedMaybe n -- bankTransferMigrateAccountRequestSecret :: Maybe Text
    <*> arbitrary -- bankTransferMigrateAccountRequestAccountNumber :: Text
    <*> arbitrary -- bankTransferMigrateAccountRequestRoutingNumber :: Text
    <*> arbitrary -- bankTransferMigrateAccountRequestAccountType :: Text
  
instance Arbitrary BankTransferMigrateAccountResponse where
  arbitrary = sized genBankTransferMigrateAccountResponse

genBankTransferMigrateAccountResponse :: Int -> Gen BankTransferMigrateAccountResponse
genBankTransferMigrateAccountResponse n =
  BankTransferMigrateAccountResponse
    <$> arbitrary -- bankTransferMigrateAccountResponseAccessToken :: Text
    <*> arbitrary -- bankTransferMigrateAccountResponseAccountId :: Text
    <*> arbitrary -- bankTransferMigrateAccountResponseRequestId :: Text
  
instance Arbitrary BankTransferReceiverDetails where
  arbitrary = sized genBankTransferReceiverDetails

genBankTransferReceiverDetails :: Int -> Gen BankTransferReceiverDetails
genBankTransferReceiverDetails n =
  BankTransferReceiverDetails
    <$> arbitraryReducedMaybe n -- bankTransferReceiverDetailsAvailableBalance :: Maybe E'AvailableBalance
  
instance Arbitrary BankTransferUser where
  arbitrary = sized genBankTransferUser

genBankTransferUser :: Int -> Gen BankTransferUser
genBankTransferUser n =
  BankTransferUser
    <$> arbitrary -- bankTransferUserLegalName :: Text
    <*> arbitraryReducedMaybe n -- bankTransferUserEmailAddress :: Maybe Text
    <*> arbitraryReducedMaybe n -- bankTransferUserRoutingNumber :: Maybe Text
  
instance Arbitrary CategoriesGetResponse where
  arbitrary = sized genCategoriesGetResponse

genCategoriesGetResponse :: Int -> Gen CategoriesGetResponse
genCategoriesGetResponse n =
  CategoriesGetResponse
    <$> arbitraryReduced n -- categoriesGetResponseCategories :: [Category]
    <*> arbitrary -- categoriesGetResponseRequestId :: Text
  
instance Arbitrary Category where
  arbitrary = sized genCategory

genCategory :: Int -> Gen Category
genCategory n =
  Category
    <$> arbitrary -- categoryCategoryId :: Text
    <*> arbitrary -- categoryGroup :: Text
    <*> arbitrary -- categoryHierarchy :: [Text]
  
instance Arbitrary Cause where
  arbitrary = sized genCause

genCause :: Int -> Gen Cause
genCause n =
  Cause
    <$> arbitrary -- causeItemId :: Text
    <*> arbitraryReduced n -- causeError :: Error
  
instance Arbitrary CreditCardLiability where
  arbitrary = sized genCreditCardLiability

genCreditCardLiability :: Int -> Gen CreditCardLiability
genCreditCardLiability n =
  CreditCardLiability
    <$> arbitraryReducedMaybe n -- creditCardLiabilityAccountId :: Maybe Text
    <*> arbitraryReduced n -- creditCardLiabilityAprs :: [APR]
    <*> arbitraryReducedMaybe n -- creditCardLiabilityIsOverdue :: Maybe Bool
    <*> arbitrary -- creditCardLiabilityLastPaymentAmount :: Double
    <*> arbitrary -- creditCardLiabilityLastPaymentDate :: Text
    <*> arbitrary -- creditCardLiabilityLastStatementBalance :: Double
    <*> arbitrary -- creditCardLiabilityLastStatementIssueDate :: Text
    <*> arbitrary -- creditCardLiabilityMinimumPaymentAmount :: Double
    <*> arbitrary -- creditCardLiabilityNextPaymentDueDate :: Text
  
instance Arbitrary CreditFilter where
  arbitrary = sized genCreditFilter

genCreditFilter :: Int -> Gen CreditFilter
genCreditFilter n =
  CreditFilter
    <$> arbitraryReduced n -- creditFilterAccountSubtypes :: [AccountSubtype]
  
instance Arbitrary DefaultUpdateWebhook where
  arbitrary = sized genDefaultUpdateWebhook

genDefaultUpdateWebhook :: Int -> Gen DefaultUpdateWebhook
genDefaultUpdateWebhook n =
  DefaultUpdateWebhook
    <$> arbitrary -- defaultUpdateWebhookWebhookType :: Text
    <*> arbitrary -- defaultUpdateWebhookWebhookCode :: Text
    <*> arbitraryReducedMaybe n -- defaultUpdateWebhookError :: Maybe Error
    <*> arbitrary -- defaultUpdateWebhookNewTransactions :: Double
    <*> arbitrary -- defaultUpdateWebhookItemId :: Text
  
instance Arbitrary DepositSwitchAddressData where
  arbitrary = sized genDepositSwitchAddressData

genDepositSwitchAddressData :: Int -> Gen DepositSwitchAddressData
genDepositSwitchAddressData n =
  DepositSwitchAddressData
    <$> arbitrary -- depositSwitchAddressDataCity :: Text
    <*> arbitrary -- depositSwitchAddressDataRegion :: Text
    <*> arbitrary -- depositSwitchAddressDataStreet :: Text
    <*> arbitrary -- depositSwitchAddressDataPostalCode :: Text
    <*> arbitrary -- depositSwitchAddressDataCountry :: Text
  
instance Arbitrary DepositSwitchAltCreateRequest where
  arbitrary = sized genDepositSwitchAltCreateRequest

genDepositSwitchAltCreateRequest :: Int -> Gen DepositSwitchAltCreateRequest
genDepositSwitchAltCreateRequest n =
  DepositSwitchAltCreateRequest
    <$> arbitraryReducedMaybe n -- depositSwitchAltCreateRequestClientId :: Maybe Text
    <*> arbitraryReducedMaybe n -- depositSwitchAltCreateRequestSecret :: Maybe Text
    <*> arbitraryReduced n -- depositSwitchAltCreateRequestTargetAccount :: DepositSwitchTargetAccount
    <*> arbitraryReduced n -- depositSwitchAltCreateRequestTargetUser :: DepositSwitchTargetUser
  
instance Arbitrary DepositSwitchAltCreateResponse where
  arbitrary = sized genDepositSwitchAltCreateResponse

genDepositSwitchAltCreateResponse :: Int -> Gen DepositSwitchAltCreateResponse
genDepositSwitchAltCreateResponse n =
  DepositSwitchAltCreateResponse
    <$> arbitrary -- depositSwitchAltCreateResponseDepositSwitchId :: Text
    <*> arbitrary -- depositSwitchAltCreateResponseRequestId :: Text
  
instance Arbitrary DepositSwitchCreateRequest where
  arbitrary = sized genDepositSwitchCreateRequest

genDepositSwitchCreateRequest :: Int -> Gen DepositSwitchCreateRequest
genDepositSwitchCreateRequest n =
  DepositSwitchCreateRequest
    <$> arbitraryReducedMaybe n -- depositSwitchCreateRequestClientId :: Maybe Text
    <*> arbitraryReducedMaybe n -- depositSwitchCreateRequestSecret :: Maybe Text
    <*> arbitrary -- depositSwitchCreateRequestTargetAccessToken :: Text
    <*> arbitrary -- depositSwitchCreateRequestTargetAccountId :: Text
  
instance Arbitrary DepositSwitchCreateResponse where
  arbitrary = sized genDepositSwitchCreateResponse

genDepositSwitchCreateResponse :: Int -> Gen DepositSwitchCreateResponse
genDepositSwitchCreateResponse n =
  DepositSwitchCreateResponse
    <$> arbitrary -- depositSwitchCreateResponseDepositSwitchId :: Text
    <*> arbitrary -- depositSwitchCreateResponseRequestId :: Text
  
instance Arbitrary DepositSwitchGetRequest where
  arbitrary = sized genDepositSwitchGetRequest

genDepositSwitchGetRequest :: Int -> Gen DepositSwitchGetRequest
genDepositSwitchGetRequest n =
  DepositSwitchGetRequest
    <$> arbitraryReducedMaybe n -- depositSwitchGetRequestClientId :: Maybe Text
    <*> arbitraryReducedMaybe n -- depositSwitchGetRequestSecret :: Maybe Text
    <*> arbitrary -- depositSwitchGetRequestDepositSwitchId :: Text
  
instance Arbitrary DepositSwitchGetResponse where
  arbitrary = sized genDepositSwitchGetResponse

genDepositSwitchGetResponse :: Int -> Gen DepositSwitchGetResponse
genDepositSwitchGetResponse n =
  DepositSwitchGetResponse
    <$> arbitrary -- depositSwitchGetResponseDepositSwitchId :: Text
    <*> arbitrary -- depositSwitchGetResponseTargetAccountId :: Text
    <*> arbitrary -- depositSwitchGetResponseTargetItemId :: Text
    <*> arbitrary -- depositSwitchGetResponseState :: E'State
    <*> arbitrary -- depositSwitchGetResponseAccountHasMultipleAllocations :: Bool
    <*> arbitrary -- depositSwitchGetResponseIsAllocatedRemainder :: Bool
    <*> arbitrary -- depositSwitchGetResponsePercentAllocated :: Double
    <*> arbitrary -- depositSwitchGetResponseAmountAllocated :: Double
    <*> arbitraryReduced n -- depositSwitchGetResponseDateCreated :: Date
    <*> arbitraryReduced n -- depositSwitchGetResponseDateCompleted :: Date
    <*> arbitrary -- depositSwitchGetResponseRequestId :: Text
  
instance Arbitrary DepositSwitchTargetAccount where
  arbitrary = sized genDepositSwitchTargetAccount

genDepositSwitchTargetAccount :: Int -> Gen DepositSwitchTargetAccount
genDepositSwitchTargetAccount n =
  DepositSwitchTargetAccount
    <$> arbitrary -- depositSwitchTargetAccountAccountNumber :: Text
    <*> arbitrary -- depositSwitchTargetAccountRoutingNumber :: Text
    <*> arbitrary -- depositSwitchTargetAccountAccountName :: Text
    <*> arbitrary -- depositSwitchTargetAccountAccountSubtype :: E'AccountSubtype
  
instance Arbitrary DepositSwitchTargetUser where
  arbitrary = sized genDepositSwitchTargetUser

genDepositSwitchTargetUser :: Int -> Gen DepositSwitchTargetUser
genDepositSwitchTargetUser n =
  DepositSwitchTargetUser
    <$> arbitrary -- depositSwitchTargetUserGivenName :: Text
    <*> arbitrary -- depositSwitchTargetUserFamilyName :: Text
    <*> arbitrary -- depositSwitchTargetUserPhone :: Text
    <*> arbitrary -- depositSwitchTargetUserEmail :: Text
    <*> arbitraryReducedMaybe n -- depositSwitchTargetUserAddress :: Maybe DepositSwitchAddressData
    <*> arbitraryReducedMaybe n -- depositSwitchTargetUserTaxPayerId :: Maybe Text
  
instance Arbitrary DepositSwitchTokenCreateRequest where
  arbitrary = sized genDepositSwitchTokenCreateRequest

genDepositSwitchTokenCreateRequest :: Int -> Gen DepositSwitchTokenCreateRequest
genDepositSwitchTokenCreateRequest n =
  DepositSwitchTokenCreateRequest
    <$> arbitraryReducedMaybe n -- depositSwitchTokenCreateRequestClientId :: Maybe Text
    <*> arbitraryReducedMaybe n -- depositSwitchTokenCreateRequestSecret :: Maybe Text
    <*> arbitrary -- depositSwitchTokenCreateRequestDepositSwitchId :: Text
  
instance Arbitrary DepositSwitchTokenCreateResponse where
  arbitrary = sized genDepositSwitchTokenCreateResponse

genDepositSwitchTokenCreateResponse :: Int -> Gen DepositSwitchTokenCreateResponse
genDepositSwitchTokenCreateResponse n =
  DepositSwitchTokenCreateResponse
    <$> arbitrary -- depositSwitchTokenCreateResponseDepositSwitchToken :: Text
    <*> arbitrary -- depositSwitchTokenCreateResponseDepositSwitchTokenExpirationTime :: Text
    <*> arbitrary -- depositSwitchTokenCreateResponseRequestId :: Text
  
instance Arbitrary DepositoryFilter where
  arbitrary = sized genDepositoryFilter

genDepositoryFilter :: Int -> Gen DepositoryFilter
genDepositoryFilter n =
  DepositoryFilter
    <$> arbitraryReduced n -- depositoryFilterAccountSubtypes :: [AccountSubtype]
  
instance Arbitrary Email where
  arbitrary = sized genEmail

genEmail :: Int -> Gen Email
genEmail n =
  Email
    <$> arbitrary -- emailData :: Text
    <*> arbitrary -- emailPrimary :: Bool
    <*> arbitrary -- emailType :: E'Type2
  
instance Arbitrary Employee where
  arbitrary = sized genEmployee

genEmployee :: Int -> Gen Employee
genEmployee n =
  Employee
    <$> arbitraryReducedMaybe n -- employeeName :: Maybe Text
    <*> arbitraryReducedMaybe n -- employeeAddress :: Maybe NullableAddressData
    <*> arbitraryReducedMaybe n -- employeeSsnMasked :: Maybe Text
  
instance Arbitrary EmployeeIncomeSummaryFieldString where
  arbitrary = sized genEmployeeIncomeSummaryFieldString

genEmployeeIncomeSummaryFieldString :: Int -> Gen EmployeeIncomeSummaryFieldString
genEmployeeIncomeSummaryFieldString n =
  EmployeeIncomeSummaryFieldString
    <$> arbitrary -- employeeIncomeSummaryFieldStringValue :: Text
    <*> arbitraryReduced n -- employeeIncomeSummaryFieldStringVerificationStatus :: VerificationStatus
  
instance Arbitrary Employer where
  arbitrary = sized genEmployer

genEmployer :: Int -> Gen Employer
genEmployer n =
  Employer
    <$> arbitrary -- employerEmployerId :: Text
    <*> arbitrary -- employerName :: Text
    <*> arbitraryReducedMaybe n -- employerAddress :: Maybe NullableAddressData
    <*> arbitraryReducedMaybe n -- employerConfidenceScore :: Maybe Double
  
instance Arbitrary EmployerIncomeSummaryFieldString where
  arbitrary = sized genEmployerIncomeSummaryFieldString

genEmployerIncomeSummaryFieldString :: Int -> Gen EmployerIncomeSummaryFieldString
genEmployerIncomeSummaryFieldString n =
  EmployerIncomeSummaryFieldString
    <$> arbitrary -- employerIncomeSummaryFieldStringValue :: Text
    <*> arbitraryReduced n -- employerIncomeSummaryFieldStringVerificationStatus :: VerificationStatus
  
instance Arbitrary EmployersSearchRequest where
  arbitrary = sized genEmployersSearchRequest

genEmployersSearchRequest :: Int -> Gen EmployersSearchRequest
genEmployersSearchRequest n =
  EmployersSearchRequest
    <$> arbitraryReducedMaybe n -- employersSearchRequestClientId :: Maybe Text
    <*> arbitraryReducedMaybe n -- employersSearchRequestSecret :: Maybe Text
    <*> arbitrary -- employersSearchRequestQuery :: Text
    <*> arbitrary -- employersSearchRequestProducts :: [Text]
  
instance Arbitrary EmployersSearchResponse where
  arbitrary = sized genEmployersSearchResponse

genEmployersSearchResponse :: Int -> Gen EmployersSearchResponse
genEmployersSearchResponse n =
  EmployersSearchResponse
    <$> arbitraryReduced n -- employersSearchResponseEmployers :: [Employer]
    <*> arbitrary -- employersSearchResponseRequestId :: Text
  
instance Arbitrary Error where
  arbitrary = sized genError

genError :: Int -> Gen Error
genError n =
  Error
    <$> arbitrary -- errorErrorType :: E'ErrorType
    <*> arbitrary -- errorErrorCode :: Text
    <*> arbitrary -- errorErrorMessage :: Text
    <*> arbitraryReducedMaybe n -- errorDisplayMessage :: Maybe Text
    <*> arbitrary -- errorRequestId :: Text
    <*> arbitraryReducedMaybe n -- errorCauses :: Maybe [A.Value]
    <*> arbitraryReducedMaybe n -- errorStatus :: Maybe Double
    <*> arbitraryReducedMaybe n -- errorDocumentationUrl :: Maybe Text
    <*> arbitraryReducedMaybe n -- errorSuggestedAction :: Maybe Text
  
instance Arbitrary ExternalPaymentSchedule where
  arbitrary = sized genExternalPaymentSchedule

genExternalPaymentSchedule :: Int -> Gen ExternalPaymentSchedule
genExternalPaymentSchedule n =
  ExternalPaymentSchedule
    <$> arbitrary -- externalPaymentScheduleInterval :: Text
    <*> arbitrary -- externalPaymentScheduleIntervalExecutionDay :: Double
    <*> arbitraryReduced n -- externalPaymentScheduleStartDate :: Date
    <*> arbitraryReducedMaybe n -- externalPaymentScheduleEndDate :: Maybe Date
  
instance Arbitrary ExternalPaymentScheduleGet where
  arbitrary = sized genExternalPaymentScheduleGet

genExternalPaymentScheduleGet :: Int -> Gen ExternalPaymentScheduleGet
genExternalPaymentScheduleGet n =
  ExternalPaymentScheduleGet
    <$> arbitraryReducedMaybe n -- externalPaymentScheduleGetAdjustedStartDate :: Maybe Date
    <*> arbitrary -- externalPaymentScheduleGetInterval :: Text
    <*> arbitrary -- externalPaymentScheduleGetIntervalExecutionDay :: Double
    <*> arbitraryReduced n -- externalPaymentScheduleGetStartDate :: Date
    <*> arbitraryReducedMaybe n -- externalPaymentScheduleGetEndDate :: Maybe Date
  
instance Arbitrary HealthIncident where
  arbitrary = sized genHealthIncident

genHealthIncident :: Int -> Gen HealthIncident
genHealthIncident n =
  HealthIncident
    <$> arbitraryReducedMaybe n -- healthIncidentStartDate :: Maybe Text
    <*> arbitraryReducedMaybe n -- healthIncidentEndDate :: Maybe Text
    <*> arbitraryReducedMaybe n -- healthIncidentTitle :: Maybe Text
    <*> arbitraryReducedMaybe n -- healthIncidentIncidentUpdates :: Maybe [IncidentUpdate]
  
instance Arbitrary HistoricalBalance where
  arbitrary = sized genHistoricalBalance

genHistoricalBalance :: Int -> Gen HistoricalBalance
genHistoricalBalance n =
  HistoricalBalance
    <$> arbitrary -- historicalBalanceDate :: Text
    <*> arbitrary -- historicalBalanceCurrent :: Double
    <*> arbitraryReducedMaybe n -- historicalBalanceIsoCurrencyCode :: Maybe Text
    <*> arbitraryReducedMaybe n -- historicalBalanceUnofficialCurrencyCode :: Maybe Text
  
instance Arbitrary HistoricalUpdateWebhook where
  arbitrary = sized genHistoricalUpdateWebhook

genHistoricalUpdateWebhook :: Int -> Gen HistoricalUpdateWebhook
genHistoricalUpdateWebhook n =
  HistoricalUpdateWebhook
    <$> arbitrary -- historicalUpdateWebhookWebhookType :: Text
    <*> arbitrary -- historicalUpdateWebhookWebhookCode :: Text
    <*> arbitraryReducedMaybe n -- historicalUpdateWebhookError :: Maybe Error
    <*> arbitrary -- historicalUpdateWebhookNewTransactions :: Double
    <*> arbitrary -- historicalUpdateWebhookItemId :: Text
  
instance Arbitrary Holding where
  arbitrary = sized genHolding

genHolding :: Int -> Gen Holding
genHolding n =
  Holding
    <$> arbitrary -- holdingAccountId :: Text
    <*> arbitrary -- holdingSecurityId :: Text
    <*> arbitrary -- holdingInstitutionPrice :: Double
    <*> arbitraryReducedMaybe n -- holdingInstitutionPriceAsOf :: Maybe Text
    <*> arbitrary -- holdingInstitutionValue :: Double
    <*> arbitraryReducedMaybe n -- holdingCostBasis :: Maybe Double
    <*> arbitrary -- holdingQuantity :: Double
    <*> arbitraryReducedMaybe n -- holdingIsoCurrencyCode :: Maybe Text
    <*> arbitraryReducedMaybe n -- holdingUnofficialCurrencyCode :: Maybe Text
  
instance Arbitrary HoldingsDefaultUpdateWebhook where
  arbitrary = sized genHoldingsDefaultUpdateWebhook

genHoldingsDefaultUpdateWebhook :: Int -> Gen HoldingsDefaultUpdateWebhook
genHoldingsDefaultUpdateWebhook n =
  HoldingsDefaultUpdateWebhook
    <$> arbitrary -- holdingsDefaultUpdateWebhookWebhookType :: Text
    <*> arbitrary -- holdingsDefaultUpdateWebhookWebhookCode :: Text
    <*> arbitrary -- holdingsDefaultUpdateWebhookItemId :: Text
    <*> arbitraryReducedMaybe n -- holdingsDefaultUpdateWebhookError :: Maybe Error
    <*> arbitrary -- holdingsDefaultUpdateWebhookNewHoldings :: Double
    <*> arbitrary -- holdingsDefaultUpdateWebhookUpdatedHoldings :: Double
  
instance Arbitrary IdentityGetRequest where
  arbitrary = sized genIdentityGetRequest

genIdentityGetRequest :: Int -> Gen IdentityGetRequest
genIdentityGetRequest n =
  IdentityGetRequest
    <$> arbitraryReducedMaybe n -- identityGetRequestClientId :: Maybe Text
    <*> arbitraryReducedMaybe n -- identityGetRequestSecret :: Maybe Text
    <*> arbitrary -- identityGetRequestAccessToken :: Text
    <*> arbitraryReducedMaybe n -- identityGetRequestOptions :: Maybe IdentityGetRequestOptions
  
instance Arbitrary IdentityGetRequestOptions where
  arbitrary = sized genIdentityGetRequestOptions

genIdentityGetRequestOptions :: Int -> Gen IdentityGetRequestOptions
genIdentityGetRequestOptions n =
  IdentityGetRequestOptions
    <$> arbitraryReducedMaybe n -- identityGetRequestOptionsAccountIds :: Maybe [Text]
  
instance Arbitrary IdentityGetResponse where
  arbitrary = sized genIdentityGetResponse

genIdentityGetResponse :: Int -> Gen IdentityGetResponse
genIdentityGetResponse n =
  IdentityGetResponse
    <$> arbitraryReduced n -- identityGetResponseAccounts :: [AccountIdentity]
    <*> arbitraryReduced n -- identityGetResponseItem :: Item
    <*> arbitrary -- identityGetResponseRequestId :: Text
  
instance Arbitrary IncidentUpdate where
  arbitrary = sized genIncidentUpdate

genIncidentUpdate :: Int -> Gen IncidentUpdate
genIncidentUpdate n =
  IncidentUpdate
    <$> arbitraryReducedMaybe n -- incidentUpdateDescription :: Maybe Text
    <*> arbitraryReducedMaybe n -- incidentUpdateStatus :: Maybe E'Status3
    <*> arbitraryReducedMaybe n -- incidentUpdateUpdatedDate :: Maybe Text
  
instance Arbitrary IncomeBreakdown where
  arbitrary = sized genIncomeBreakdown

genIncomeBreakdown :: Int -> Gen IncomeBreakdown
genIncomeBreakdown n =
  IncomeBreakdown
    <$> arbitraryReducedMaybe n -- incomeBreakdownType :: Maybe Text
    <*> arbitraryReducedMaybe n -- incomeBreakdownRate :: Maybe Double
    <*> arbitraryReducedMaybe n -- incomeBreakdownHours :: Maybe Double
    <*> arbitraryReducedMaybe n -- incomeBreakdownTotal :: Maybe Double
  
instance Arbitrary IncomeSummary where
  arbitrary = sized genIncomeSummary

genIncomeSummary :: Int -> Gen IncomeSummary
genIncomeSummary n =
  IncomeSummary
    <$> arbitraryReducedMaybe n -- incomeSummaryEmployerName :: Maybe EmployerIncomeSummaryFieldString
    <*> arbitraryReducedMaybe n -- incomeSummaryEmployeeName :: Maybe EmployeeIncomeSummaryFieldString
    <*> arbitraryReducedMaybe n -- incomeSummaryYtdGrossIncome :: Maybe YTDGrossIncomeSummaryFieldNumber
    <*> arbitraryReducedMaybe n -- incomeSummaryYtdNetIncome :: Maybe YTDNetIncomeSummaryFieldNumber
    <*> arbitraryReducedMaybe n -- incomeSummaryPayFrequency :: Maybe PayFrequency
    <*> arbitraryReducedMaybe n -- incomeSummaryProjectedWage :: Maybe ProjectedIncomeSummaryFieldNumber
    <*> arbitraryReducedMaybe n -- incomeSummaryVerifiedTransaction :: Maybe TransactionData
  
instance Arbitrary IncomeSummaryFieldNumber where
  arbitrary = sized genIncomeSummaryFieldNumber

genIncomeSummaryFieldNumber :: Int -> Gen IncomeSummaryFieldNumber
genIncomeSummaryFieldNumber n =
  IncomeSummaryFieldNumber
    <$> arbitrary -- incomeSummaryFieldNumberValue :: Double
    <*> arbitraryReduced n -- incomeSummaryFieldNumberVerificationStatus :: VerificationStatus
  
instance Arbitrary IncomeSummaryFieldString where
  arbitrary = sized genIncomeSummaryFieldString

genIncomeSummaryFieldString :: Int -> Gen IncomeSummaryFieldString
genIncomeSummaryFieldString n =
  IncomeSummaryFieldString
    <$> arbitrary -- incomeSummaryFieldStringValue :: Text
    <*> arbitraryReduced n -- incomeSummaryFieldStringVerificationStatus :: VerificationStatus
  
instance Arbitrary IncomeVerificationCreateRequest where
  arbitrary = sized genIncomeVerificationCreateRequest

genIncomeVerificationCreateRequest :: Int -> Gen IncomeVerificationCreateRequest
genIncomeVerificationCreateRequest n =
  IncomeVerificationCreateRequest
    <$> arbitraryReducedMaybe n -- incomeVerificationCreateRequestClientId :: Maybe Text
    <*> arbitraryReducedMaybe n -- incomeVerificationCreateRequestSecret :: Maybe Text
    <*> arbitrary -- incomeVerificationCreateRequestWebhook :: Text
  
instance Arbitrary IncomeVerificationCreateResponse where
  arbitrary = sized genIncomeVerificationCreateResponse

genIncomeVerificationCreateResponse :: Int -> Gen IncomeVerificationCreateResponse
genIncomeVerificationCreateResponse n =
  IncomeVerificationCreateResponse
    <$> arbitrary -- incomeVerificationCreateResponseIncomeVerificationId :: Text
    <*> arbitrary -- incomeVerificationCreateResponseRequestId :: Text
  
instance Arbitrary IncomeVerificationDocumentsDownloadRequest where
  arbitrary = sized genIncomeVerificationDocumentsDownloadRequest

genIncomeVerificationDocumentsDownloadRequest :: Int -> Gen IncomeVerificationDocumentsDownloadRequest
genIncomeVerificationDocumentsDownloadRequest n =
  IncomeVerificationDocumentsDownloadRequest
    <$> arbitraryReducedMaybe n -- incomeVerificationDocumentsDownloadRequestClientId :: Maybe Text
    <*> arbitraryReducedMaybe n -- incomeVerificationDocumentsDownloadRequestSecret :: Maybe Text
    <*> arbitrary -- incomeVerificationDocumentsDownloadRequestIncomeVerificationId :: Text
  
instance Arbitrary IncomeVerificationDocumentsDownloadResponse where
  arbitrary = sized genIncomeVerificationDocumentsDownloadResponse

genIncomeVerificationDocumentsDownloadResponse :: Int -> Gen IncomeVerificationDocumentsDownloadResponse
genIncomeVerificationDocumentsDownloadResponse n =
  IncomeVerificationDocumentsDownloadResponse
    <$> arbitrary -- incomeVerificationDocumentsDownloadResponseId :: Text
  
instance Arbitrary IncomeVerificationPaystubGetRequest where
  arbitrary = sized genIncomeVerificationPaystubGetRequest

genIncomeVerificationPaystubGetRequest :: Int -> Gen IncomeVerificationPaystubGetRequest
genIncomeVerificationPaystubGetRequest n =
  IncomeVerificationPaystubGetRequest
    <$> arbitraryReducedMaybe n -- incomeVerificationPaystubGetRequestClientId :: Maybe Text
    <*> arbitraryReducedMaybe n -- incomeVerificationPaystubGetRequestSecret :: Maybe Text
    <*> arbitrary -- incomeVerificationPaystubGetRequestIncomeVerificationId :: Text
  
instance Arbitrary IncomeVerificationPaystubGetResponse where
  arbitrary = sized genIncomeVerificationPaystubGetResponse

genIncomeVerificationPaystubGetResponse :: Int -> Gen IncomeVerificationPaystubGetResponse
genIncomeVerificationPaystubGetResponse n =
  IncomeVerificationPaystubGetResponse
    <$> arbitraryReducedMaybe n -- incomeVerificationPaystubGetResponsePaystub :: Maybe Paystub
    <*> arbitraryReducedMaybe n -- incomeVerificationPaystubGetResponseRequestId :: Maybe Text
  
instance Arbitrary IncomeVerificationStatusWebhook where
  arbitrary = sized genIncomeVerificationStatusWebhook

genIncomeVerificationStatusWebhook :: Int -> Gen IncomeVerificationStatusWebhook
genIncomeVerificationStatusWebhook n =
  IncomeVerificationStatusWebhook
    <$> arbitrary -- incomeVerificationStatusWebhookWebhookType :: Text
    <*> arbitrary -- incomeVerificationStatusWebhookWebhookCode :: Text
    <*> arbitrary -- incomeVerificationStatusWebhookIncomeVerificationId :: Text
    <*> arbitrary -- incomeVerificationStatusWebhookVerificationStatus :: Text
  
instance Arbitrary IncomeVerificationSummaryGetRequest where
  arbitrary = sized genIncomeVerificationSummaryGetRequest

genIncomeVerificationSummaryGetRequest :: Int -> Gen IncomeVerificationSummaryGetRequest
genIncomeVerificationSummaryGetRequest n =
  IncomeVerificationSummaryGetRequest
    <$> arbitraryReducedMaybe n -- incomeVerificationSummaryGetRequestClientId :: Maybe Text
    <*> arbitraryReducedMaybe n -- incomeVerificationSummaryGetRequestSecret :: Maybe Text
    <*> arbitrary -- incomeVerificationSummaryGetRequestIncomeVerificationId :: Text
  
instance Arbitrary IncomeVerificationSummaryGetResponse where
  arbitrary = sized genIncomeVerificationSummaryGetResponse

genIncomeVerificationSummaryGetResponse :: Int -> Gen IncomeVerificationSummaryGetResponse
genIncomeVerificationSummaryGetResponse n =
  IncomeVerificationSummaryGetResponse
    <$> arbitraryReduced n -- incomeVerificationSummaryGetResponseIncomeSummaries :: [IncomeSummary]
    <*> arbitrary -- incomeVerificationSummaryGetResponseRequestId :: Text
  
instance Arbitrary IncomeVerificationWebhookStatus where
  arbitrary = sized genIncomeVerificationWebhookStatus

genIncomeVerificationWebhookStatus :: Int -> Gen IncomeVerificationWebhookStatus
genIncomeVerificationWebhookStatus n =
  IncomeVerificationWebhookStatus
    <$> arbitrary -- incomeVerificationWebhookStatusId :: Text
  
instance Arbitrary InflowModel where
  arbitrary = sized genInflowModel

genInflowModel :: Int -> Gen InflowModel
genInflowModel n =
  InflowModel
    <$> arbitrary -- inflowModelType :: Text
    <*> arbitrary -- inflowModelIncomeAmount :: Double
    <*> arbitrary -- inflowModelPaymentDayOfMonth :: Double
    <*> arbitrary -- inflowModelTransactionName :: Text
    <*> arbitrary -- inflowModelStatementDayOfMonth :: Text
  
instance Arbitrary InitialUpdateWebhook where
  arbitrary = sized genInitialUpdateWebhook

genInitialUpdateWebhook :: Int -> Gen InitialUpdateWebhook
genInitialUpdateWebhook n =
  InitialUpdateWebhook
    <$> arbitrary -- initialUpdateWebhookWebhookType :: Text
    <*> arbitrary -- initialUpdateWebhookWebhookCode :: Text
    <*> arbitraryReducedMaybe n -- initialUpdateWebhookError :: Maybe Text
    <*> arbitrary -- initialUpdateWebhookNewTransactions :: Double
    <*> arbitrary -- initialUpdateWebhookItemId :: Text
  
instance Arbitrary Institution where
  arbitrary = sized genInstitution

genInstitution :: Int -> Gen Institution
genInstitution n =
  Institution
    <$> arbitrary -- institutionInstitutionId :: Text
    <*> arbitrary -- institutionName :: Text
    <*> arbitraryReduced n -- institutionProducts :: [Products]
    <*> arbitraryReduced n -- institutionCountryCodes :: [CountryCode]
    <*> arbitraryReducedMaybe n -- institutionUrl :: Maybe Text
    <*> arbitraryReducedMaybe n -- institutionPrimaryColor :: Maybe Text
    <*> arbitraryReducedMaybe n -- institutionLogo :: Maybe Text
    <*> arbitraryReducedMaybe n -- institutionRoutingNumbers :: Maybe [Text]
    <*> arbitrary -- institutionOauth :: Bool
    <*> arbitraryReducedMaybe n -- institutionStatus :: Maybe InstitutionStatus
    <*> arbitraryReducedMaybe n -- institutionAuthMetadata :: Maybe InstitutionAuthMetadata
  
instance Arbitrary InstitutionAuthMetadata where
  arbitrary = sized genInstitutionAuthMetadata

genInstitutionAuthMetadata :: Int -> Gen InstitutionAuthMetadata
genInstitutionAuthMetadata n =
  InstitutionAuthMetadata
    <$> arbitraryReducedMaybe n -- institutionAuthMetadataSupportedMethods :: Maybe InstitutionSupportedMethods

instance Arbitrary InstitutionSupportedMethods where
  arbitrary = sized genInstitutionSupportedMethods

genInstitutionSupportedMethods :: Int -> Gen InstitutionSupportedMethods
genInstitutionSupportedMethods n =
  InstitutionSupportedMethods
    <$> arbitrary -- institutionSupportedMethodsAutomatedMicroDeposits :: Bool
    <*> arbitrary -- institutionSupportedMethodsInstantAuth :: Bool
    <*> arbitrary -- institutionSupportedMethodsInstantMatch :: Bool
    <*> arbitrary -- institutionSupportedMethodsInstantMicroDeposits :: Bool

instance Arbitrary InstitutionStatus where
  arbitrary = sized genInstitutionStatus

genInstitutionStatus :: Int -> Gen InstitutionStatus
genInstitutionStatus n =
  InstitutionStatus
    <$> arbitraryReduced n -- institutionStatusItemLogins :: ProductStatus
    <*> arbitraryReduced n -- institutionStatusTransactionsUpdates :: ProductStatus
    <*> arbitraryReduced n -- institutionStatusAuth :: ProductStatus
    <*> arbitraryReduced n -- institutionStatusBalance :: ProductStatus
    <*> arbitraryReduced n -- institutionStatusIdentity :: ProductStatus
    <*> arbitraryReduced n -- institutionStatusInvestmentsUpdates :: ProductStatus
    <*> arbitraryReducedMaybe n -- institutionStatusHealthIncidents :: Maybe [HealthIncident]
  
instance Arbitrary InstitutionsGetByIdRequest where
  arbitrary = sized genInstitutionsGetByIdRequest

genInstitutionsGetByIdRequest :: Int -> Gen InstitutionsGetByIdRequest
genInstitutionsGetByIdRequest n =
  InstitutionsGetByIdRequest
    <$> arbitraryReducedMaybe n -- institutionsGetByIdRequestClientId :: Maybe Text
    <*> arbitraryReducedMaybe n -- institutionsGetByIdRequestSecret :: Maybe Text
    <*> arbitrary -- institutionsGetByIdRequestInstitutionId :: Text
    <*> arbitraryReduced n -- institutionsGetByIdRequestCountryCodes :: [CountryCode]
    <*> arbitraryReducedMaybe n -- institutionsGetByIdRequestOptions :: Maybe InstitutionsGetByIdRequestOptions
  
instance Arbitrary InstitutionsGetByIdRequestOptions where
  arbitrary = sized genInstitutionsGetByIdRequestOptions

genInstitutionsGetByIdRequestOptions :: Int -> Gen InstitutionsGetByIdRequestOptions
genInstitutionsGetByIdRequestOptions n =
  InstitutionsGetByIdRequestOptions
    <$> arbitraryReducedMaybe n -- institutionsGetByIdRequestOptionsIncludeOptionalMetadata :: Maybe Bool
    <*> arbitraryReducedMaybe n -- institutionsGetByIdRequestOptionsIncludeStatus :: Maybe Bool
    <*> arbitraryReducedMaybe n -- institutionsGetByIdRequestOptionsIncludeAuthMetadata :: Maybe Bool
  
instance Arbitrary InstitutionsGetByIdResponse where
  arbitrary = sized genInstitutionsGetByIdResponse

genInstitutionsGetByIdResponse :: Int -> Gen InstitutionsGetByIdResponse
genInstitutionsGetByIdResponse n =
  InstitutionsGetByIdResponse
    <$> arbitraryReduced n -- institutionsGetByIdResponseInstitution :: Institution
    <*> arbitrary -- institutionsGetByIdResponseRequestId :: Text
  
instance Arbitrary InstitutionsGetRequest where
  arbitrary = sized genInstitutionsGetRequest

genInstitutionsGetRequest :: Int -> Gen InstitutionsGetRequest
genInstitutionsGetRequest n =
  InstitutionsGetRequest
    <$> arbitraryReducedMaybe n -- institutionsGetRequestClientId :: Maybe Text
    <*> arbitraryReducedMaybe n -- institutionsGetRequestSecret :: Maybe Text
    <*> arbitrary -- institutionsGetRequestCount :: Int
    <*> arbitrary -- institutionsGetRequestOffset :: Int
    <*> arbitraryReduced n -- institutionsGetRequestCountryCodes :: [CountryCode]
    <*> arbitraryReducedMaybe n -- institutionsGetRequestOptions :: Maybe InstitutionsGetRequestOptions
  
instance Arbitrary InstitutionsGetRequestOptions where
  arbitrary = sized genInstitutionsGetRequestOptions

genInstitutionsGetRequestOptions :: Int -> Gen InstitutionsGetRequestOptions
genInstitutionsGetRequestOptions n =
  InstitutionsGetRequestOptions
    <$> arbitraryReducedMaybe n -- institutionsGetRequestOptionsProducts :: Maybe [Products]
    <*> arbitraryReducedMaybe n -- institutionsGetRequestOptionsRoutingNumbers :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- institutionsGetRequestOptionsOauth :: Maybe Bool
    <*> arbitraryReducedMaybe n -- institutionsGetRequestOptionsIncludeOptionalMetadata :: Maybe Bool
  
instance Arbitrary InstitutionsGetResponse where
  arbitrary = sized genInstitutionsGetResponse

genInstitutionsGetResponse :: Int -> Gen InstitutionsGetResponse
genInstitutionsGetResponse n =
  InstitutionsGetResponse
    <$> arbitraryReduced n -- institutionsGetResponseInstitutions :: [Institution]
    <*> arbitrary -- institutionsGetResponseTotal :: Int
    <*> arbitrary -- institutionsGetResponseRequestId :: Text
  
instance Arbitrary InstitutionsSearchAccountFilter where
  arbitrary = sized genInstitutionsSearchAccountFilter

genInstitutionsSearchAccountFilter :: Int -> Gen InstitutionsSearchAccountFilter
genInstitutionsSearchAccountFilter n =
  InstitutionsSearchAccountFilter
    <$> arbitraryReducedMaybe n -- institutionsSearchAccountFilterLoan :: Maybe [AccountSubtype]
    <*> arbitraryReducedMaybe n -- institutionsSearchAccountFilterDepository :: Maybe [AccountSubtype]
    <*> arbitraryReducedMaybe n -- institutionsSearchAccountFilterCredit :: Maybe [AccountSubtype]
    <*> arbitraryReducedMaybe n -- institutionsSearchAccountFilterInvestment :: Maybe [AccountSubtype]
  
instance Arbitrary InstitutionsSearchRequest where
  arbitrary = sized genInstitutionsSearchRequest

genInstitutionsSearchRequest :: Int -> Gen InstitutionsSearchRequest
genInstitutionsSearchRequest n =
  InstitutionsSearchRequest
    <$> arbitraryReducedMaybe n -- institutionsSearchRequestClientId :: Maybe Text
    <*> arbitraryReducedMaybe n -- institutionsSearchRequestSecret :: Maybe Text
    <*> arbitrary -- institutionsSearchRequestQuery :: Text
    <*> arbitraryReduced n -- institutionsSearchRequestProducts :: [Products]
    <*> arbitraryReduced n -- institutionsSearchRequestCountryCodes :: [CountryCode]
    <*> arbitraryReducedMaybe n -- institutionsSearchRequestOptions :: Maybe InstitutionsSearchRequestOptions
  
instance Arbitrary InstitutionsSearchRequestOptions where
  arbitrary = sized genInstitutionsSearchRequestOptions

genInstitutionsSearchRequestOptions :: Int -> Gen InstitutionsSearchRequestOptions
genInstitutionsSearchRequestOptions n =
  InstitutionsSearchRequestOptions
    <$> arbitraryReducedMaybe n -- institutionsSearchRequestOptionsOauth :: Maybe Bool
    <*> arbitraryReducedMaybe n -- institutionsSearchRequestOptionsIncludeOptionalMetadata :: Maybe Bool
    <*> arbitraryReducedMaybe n -- institutionsSearchRequestOptionsAccountFilter :: Maybe InstitutionsSearchAccountFilter
  
instance Arbitrary InstitutionsSearchResponse where
  arbitrary = sized genInstitutionsSearchResponse

genInstitutionsSearchResponse :: Int -> Gen InstitutionsSearchResponse
genInstitutionsSearchResponse n =
  InstitutionsSearchResponse
    <$> arbitraryReduced n -- institutionsSearchResponseInstitutions :: [Institution]
    <*> arbitrary -- institutionsSearchResponseRequestId :: Text
  
instance Arbitrary InvestmentFilter where
  arbitrary = sized genInvestmentFilter

genInvestmentFilter :: Int -> Gen InvestmentFilter
genInvestmentFilter n =
  InvestmentFilter
    <$> arbitraryReduced n -- investmentFilterAccountSubtypes :: [AccountSubtype]
  
instance Arbitrary InvestmentHoldingsGetRequestOptions where
  arbitrary = sized genInvestmentHoldingsGetRequestOptions

genInvestmentHoldingsGetRequestOptions :: Int -> Gen InvestmentHoldingsGetRequestOptions
genInvestmentHoldingsGetRequestOptions n =
  InvestmentHoldingsGetRequestOptions
    <$> arbitraryReducedMaybe n -- investmentHoldingsGetRequestOptionsAccountIds :: Maybe [Text]
  
instance Arbitrary InvestmentTransaction where
  arbitrary = sized genInvestmentTransaction

genInvestmentTransaction :: Int -> Gen InvestmentTransaction
genInvestmentTransaction n =
  InvestmentTransaction
    <$> arbitrary -- investmentTransactionInvestmentTransactionId :: Text
    <*> arbitraryReducedMaybe n -- investmentTransactionCancelTransactionId :: Maybe Text
    <*> arbitrary -- investmentTransactionAccountId :: Text
    <*> arbitraryReducedMaybe n -- investmentTransactionSecurityId :: Maybe Text
    <*> arbitrary -- investmentTransactionDate :: Text
    <*> arbitrary -- investmentTransactionName :: Text
    <*> arbitrary -- investmentTransactionQuantity :: Double
    <*> arbitrary -- investmentTransactionAmount :: Double
    <*> arbitrary -- investmentTransactionPrice :: Double
    <*> arbitraryReducedMaybe n -- investmentTransactionFees :: Maybe Double
    <*> arbitrary -- investmentTransactionType :: E'Type5
    <*> arbitrary -- investmentTransactionSubtype :: E'Subtype
    <*> arbitraryReducedMaybe n -- investmentTransactionIsoCurrencyCode :: Maybe Text
    <*> arbitraryReducedMaybe n -- investmentTransactionUnofficialCurrencyCode :: Maybe Text
  
instance Arbitrary InvestmentsDefaultUpdateWebhook where
  arbitrary = sized genInvestmentsDefaultUpdateWebhook

genInvestmentsDefaultUpdateWebhook :: Int -> Gen InvestmentsDefaultUpdateWebhook
genInvestmentsDefaultUpdateWebhook n =
  InvestmentsDefaultUpdateWebhook
    <$> arbitrary -- investmentsDefaultUpdateWebhookWebhookType :: Text
    <*> arbitrary -- investmentsDefaultUpdateWebhookWebhookCode :: Text
    <*> arbitrary -- investmentsDefaultUpdateWebhookItemId :: Text
    <*> arbitraryReducedMaybe n -- investmentsDefaultUpdateWebhookError :: Maybe Error
    <*> arbitrary -- investmentsDefaultUpdateWebhookNewInvestmentsTransactions :: Double
    <*> arbitrary -- investmentsDefaultUpdateWebhookCanceledInvestmentsTransactions :: Double
  
instance Arbitrary InvestmentsHoldingsGetRequest where
  arbitrary = sized genInvestmentsHoldingsGetRequest

genInvestmentsHoldingsGetRequest :: Int -> Gen InvestmentsHoldingsGetRequest
genInvestmentsHoldingsGetRequest n =
  InvestmentsHoldingsGetRequest
    <$> arbitraryReducedMaybe n -- investmentsHoldingsGetRequestClientId :: Maybe Text
    <*> arbitraryReducedMaybe n -- investmentsHoldingsGetRequestSecret :: Maybe Text
    <*> arbitrary -- investmentsHoldingsGetRequestAccessToken :: Text
    <*> arbitraryReducedMaybe n -- investmentsHoldingsGetRequestOptions :: Maybe InvestmentHoldingsGetRequestOptions
  
instance Arbitrary InvestmentsHoldingsGetResponse where
  arbitrary = sized genInvestmentsHoldingsGetResponse

genInvestmentsHoldingsGetResponse :: Int -> Gen InvestmentsHoldingsGetResponse
genInvestmentsHoldingsGetResponse n =
  InvestmentsHoldingsGetResponse
    <$> arbitraryReduced n -- investmentsHoldingsGetResponseAccounts :: [AccountBase]
    <*> arbitraryReduced n -- investmentsHoldingsGetResponseHoldings :: [Holding]
    <*> arbitraryReduced n -- investmentsHoldingsGetResponseSecurities :: [Security]
    <*> arbitraryReduced n -- investmentsHoldingsGetResponseItem :: Item
    <*> arbitrary -- investmentsHoldingsGetResponseRequestId :: Text
  
instance Arbitrary InvestmentsTransactionsGetRequest where
  arbitrary = sized genInvestmentsTransactionsGetRequest

genInvestmentsTransactionsGetRequest :: Int -> Gen InvestmentsTransactionsGetRequest
genInvestmentsTransactionsGetRequest n =
  InvestmentsTransactionsGetRequest
    <$> arbitraryReducedMaybe n -- investmentsTransactionsGetRequestClientId :: Maybe Text
    <*> arbitraryReducedMaybe n -- investmentsTransactionsGetRequestSecret :: Maybe Text
    <*> arbitrary -- investmentsTransactionsGetRequestAccessToken :: Text
    <*> arbitraryReduced n -- investmentsTransactionsGetRequestStartDate :: Date
    <*> arbitraryReduced n -- investmentsTransactionsGetRequestEndDate :: Date
    <*> arbitraryReducedMaybe n -- investmentsTransactionsGetRequestOptions :: Maybe InvestmentsTransactionsGetRequestOptions
  
instance Arbitrary InvestmentsTransactionsGetRequestOptions where
  arbitrary = sized genInvestmentsTransactionsGetRequestOptions

genInvestmentsTransactionsGetRequestOptions :: Int -> Gen InvestmentsTransactionsGetRequestOptions
genInvestmentsTransactionsGetRequestOptions n =
  InvestmentsTransactionsGetRequestOptions
    <$> arbitraryReducedMaybe n -- investmentsTransactionsGetRequestOptionsAccountIds :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- investmentsTransactionsGetRequestOptionsCount :: Maybe Int
    <*> arbitraryReducedMaybe n -- investmentsTransactionsGetRequestOptionsOffset :: Maybe Int
  
instance Arbitrary InvestmentsTransactionsGetResponse where
  arbitrary = sized genInvestmentsTransactionsGetResponse

genInvestmentsTransactionsGetResponse :: Int -> Gen InvestmentsTransactionsGetResponse
genInvestmentsTransactionsGetResponse n =
  InvestmentsTransactionsGetResponse
    <$> arbitraryReduced n -- investmentsTransactionsGetResponseItem :: Item
    <*> arbitraryReduced n -- investmentsTransactionsGetResponseAccounts :: [AccountBase]
    <*> arbitraryReduced n -- investmentsTransactionsGetResponseSecurities :: [Security]
    <*> arbitraryReduced n -- investmentsTransactionsGetResponseInvestmentTransactions :: [InvestmentTransaction]
    <*> arbitrary -- investmentsTransactionsGetResponseTotalInvestmentTransactions :: Int
    <*> arbitrary -- investmentsTransactionsGetResponseRequestId :: Text
  
instance Arbitrary ItemId where
  arbitrary = ItemId <$> arbitrary
  
instance Arbitrary Item where
  arbitrary = sized genItem

genItem :: Int -> Gen Item
genItem n =
  Item
    <$> arbitrary -- itemItemId :: ItemId
    <*> arbitraryReducedMaybe n -- itemInstitutionId :: Maybe Text
    <*> arbitraryReducedMaybe n -- itemWebhook :: Maybe Text
    <*> arbitraryReducedMaybe n -- itemError :: Maybe Error
    <*> arbitraryReduced n -- itemAvailableProducts :: [Products]
    <*> arbitraryReduced n -- itemBilledProducts :: [Products]
    <*> arbitraryReducedMaybe n -- itemConsentedProducts :: Maybe [Products]
    <*> arbitraryReducedMaybe n -- itemConsentExpirationTime :: Maybe Text
    <*> arbitrary -- itemUpdateType :: E'UpdateType
  
instance Arbitrary ItemAccessTokenInvalidateRequest where
  arbitrary = sized genItemAccessTokenInvalidateRequest

genItemAccessTokenInvalidateRequest :: Int -> Gen ItemAccessTokenInvalidateRequest
genItemAccessTokenInvalidateRequest n =
  ItemAccessTokenInvalidateRequest
    <$> arbitraryReducedMaybe n -- itemAccessTokenInvalidateRequestClientId :: Maybe Text
    <*> arbitraryReducedMaybe n -- itemAccessTokenInvalidateRequestSecret :: Maybe Text
    <*> arbitrary -- itemAccessTokenInvalidateRequestAccessToken :: Text
  
instance Arbitrary ItemAccessTokenInvalidateResponse where
  arbitrary = sized genItemAccessTokenInvalidateResponse

genItemAccessTokenInvalidateResponse :: Int -> Gen ItemAccessTokenInvalidateResponse
genItemAccessTokenInvalidateResponse n =
  ItemAccessTokenInvalidateResponse
    <$> arbitrary -- itemAccessTokenInvalidateResponseNewAccessToken :: Text
    <*> arbitrary -- itemAccessTokenInvalidateResponseRequestId :: Text
  
instance Arbitrary ItemErrorWebhook where
  arbitrary = sized genItemErrorWebhook

genItemErrorWebhook :: Int -> Gen ItemErrorWebhook
genItemErrorWebhook n =
  ItemErrorWebhook
    <$> arbitrary -- itemErrorWebhookWebhookType :: Text
    <*> arbitrary -- itemErrorWebhookWebhookCode :: Text
    <*> arbitrary -- itemErrorWebhookItemId :: Text
    <*> arbitraryReduced n -- itemErrorWebhookError :: Error
  
instance Arbitrary ItemGetRequest where
  arbitrary = sized genItemGetRequest

genItemGetRequest :: Int -> Gen ItemGetRequest
genItemGetRequest n =
  ItemGetRequest
    <$> arbitraryReducedMaybe n -- itemGetRequestClientId :: Maybe Text
    <*> arbitraryReducedMaybe n -- itemGetRequestSecret :: Maybe Text
    <*> arbitrary -- itemGetRequestAccessToken :: Text
  
instance Arbitrary ItemGetResponse where
  arbitrary = sized genItemGetResponse

genItemGetResponse :: Int -> Gen ItemGetResponse
genItemGetResponse n =
  ItemGetResponse
    <$> arbitraryReduced n -- itemGetResponseItem :: Item
    <*> arbitraryReducedMaybe n -- itemGetResponseStatus :: Maybe NullableItemStatus
    <*> arbitrary -- itemGetResponseRequestId :: Text
    <*> arbitraryReducedMaybe n -- itemGetResponseAccessToken :: Maybe NullableAccessToken
  
instance Arbitrary ItemImportRequest where
  arbitrary = sized genItemImportRequest

genItemImportRequest :: Int -> Gen ItemImportRequest
genItemImportRequest n =
  ItemImportRequest
    <$> arbitraryReducedMaybe n -- itemImportRequestClientId :: Maybe Text
    <*> arbitraryReducedMaybe n -- itemImportRequestSecret :: Maybe Text
    <*> arbitraryReduced n -- itemImportRequestProducts :: [Products]
    <*> arbitraryReduced n -- itemImportRequestUserAuth :: ItemImportRequestUserAuth
    <*> arbitraryReducedMaybe n -- itemImportRequestOptions :: Maybe ItemImportRequestOptions
  
instance Arbitrary ItemImportRequestOptions where
  arbitrary = sized genItemImportRequestOptions

genItemImportRequestOptions :: Int -> Gen ItemImportRequestOptions
genItemImportRequestOptions n =
  ItemImportRequestOptions
    <$> arbitraryReducedMaybe n -- itemImportRequestOptionsWebhook :: Maybe Text
  
instance Arbitrary ItemImportRequestUserAuth where
  arbitrary = sized genItemImportRequestUserAuth

genItemImportRequestUserAuth :: Int -> Gen ItemImportRequestUserAuth
genItemImportRequestUserAuth n =
  ItemImportRequestUserAuth
    <$> arbitrary -- itemImportRequestUserAuthUserId :: Text
    <*> arbitrary -- itemImportRequestUserAuthAuthToken :: Text
  
instance Arbitrary ItemImportResponse where
  arbitrary = sized genItemImportResponse

genItemImportResponse :: Int -> Gen ItemImportResponse
genItemImportResponse n =
  ItemImportResponse
    <$> arbitrary -- itemImportResponseAccessToken :: Text
    <*> arbitrary -- itemImportResponseRequestId :: Text
  
instance Arbitrary ItemProductReadyWebhook where
  arbitrary = sized genItemProductReadyWebhook

genItemProductReadyWebhook :: Int -> Gen ItemProductReadyWebhook
genItemProductReadyWebhook n =
  ItemProductReadyWebhook
    <$> arbitrary -- itemProductReadyWebhookWebhookType :: Text
    <*> arbitrary -- itemProductReadyWebhookWebhookCode :: Text
    <*> arbitrary -- itemProductReadyWebhookItemId :: Text
    <*> arbitraryReducedMaybe n -- itemProductReadyWebhookError :: Maybe Error
  
instance Arbitrary ItemPublicTokenCreateRequest where
  arbitrary = sized genItemPublicTokenCreateRequest

genItemPublicTokenCreateRequest :: Int -> Gen ItemPublicTokenCreateRequest
genItemPublicTokenCreateRequest n =
  ItemPublicTokenCreateRequest
    <$> arbitraryReducedMaybe n -- itemPublicTokenCreateRequestClientId :: Maybe Text
    <*> arbitraryReducedMaybe n -- itemPublicTokenCreateRequestSecret :: Maybe Text
    <*> arbitrary -- itemPublicTokenCreateRequestAccessToken :: Text
  
instance Arbitrary ItemPublicTokenCreateResponse where
  arbitrary = sized genItemPublicTokenCreateResponse

genItemPublicTokenCreateResponse :: Int -> Gen ItemPublicTokenCreateResponse
genItemPublicTokenCreateResponse n =
  ItemPublicTokenCreateResponse
    <$> arbitrary -- itemPublicTokenCreateResponsePublicToken :: Text
    <*> arbitraryReducedMaybe n -- itemPublicTokenCreateResponseExpiration :: Maybe DateTime
    <*> arbitrary -- itemPublicTokenCreateResponseRequestId :: Text
  
instance Arbitrary ItemPublicTokenExchangeRequest where
  arbitrary = sized genItemPublicTokenExchangeRequest

genItemPublicTokenExchangeRequest :: Int -> Gen ItemPublicTokenExchangeRequest
genItemPublicTokenExchangeRequest n =
  ItemPublicTokenExchangeRequest
    <$> arbitraryReducedMaybe n -- itemPublicTokenExchangeRequestClientId :: Maybe Text
    <*> arbitraryReducedMaybe n -- itemPublicTokenExchangeRequestSecret :: Maybe Text
    <*> arbitrary -- itemPublicTokenExchangeRequestPublicToken :: Text
  
instance Arbitrary ItemPublicTokenExchangeResponse where
  arbitrary = sized genItemPublicTokenExchangeResponse

genItemPublicTokenExchangeResponse :: Int -> Gen ItemPublicTokenExchangeResponse
genItemPublicTokenExchangeResponse n =
  ItemPublicTokenExchangeResponse
    <$> arbitrary -- itemPublicTokenExchangeResponseAccessToken :: Text
    <*> arbitrary -- itemPublicTokenExchangeResponseItemId :: Text
    <*> arbitrary -- itemPublicTokenExchangeResponseRequestId :: Text
  
instance Arbitrary ItemRemoveRequest where
  arbitrary = sized genItemRemoveRequest

genItemRemoveRequest :: Int -> Gen ItemRemoveRequest
genItemRemoveRequest n =
  ItemRemoveRequest
    <$> arbitraryReducedMaybe n -- itemRemoveRequestClientId :: Maybe Text
    <*> arbitraryReducedMaybe n -- itemRemoveRequestSecret :: Maybe Text
    <*> arbitrary -- itemRemoveRequestAccessToken :: Text
  
instance Arbitrary ItemRemoveResponse where
  arbitrary = sized genItemRemoveResponse

genItemRemoveResponse :: Int -> Gen ItemRemoveResponse
genItemRemoveResponse n =
  ItemRemoveResponse
    <$> arbitrary -- itemRemoveResponseRequestId :: Text
  
instance Arbitrary ItemStatus where
  arbitrary = sized genItemStatus

genItemStatus :: Int -> Gen ItemStatus
genItemStatus n =
  ItemStatus
    <$> arbitraryReducedMaybe n -- itemStatusInvestments :: Maybe (Map.Map String A.Value)
    <*> arbitraryReducedMaybe n -- itemStatusTransactions :: Maybe (Map.Map String A.Value)
    <*> arbitraryReducedMaybe n -- itemStatusLastWebhook :: Maybe (Map.Map String A.Value)
  
instance Arbitrary ItemWebhookUpdateRequest where
  arbitrary = sized genItemWebhookUpdateRequest

genItemWebhookUpdateRequest :: Int -> Gen ItemWebhookUpdateRequest
genItemWebhookUpdateRequest n =
  ItemWebhookUpdateRequest
    <$> arbitraryReducedMaybe n -- itemWebhookUpdateRequestClientId :: Maybe Text
    <*> arbitraryReducedMaybe n -- itemWebhookUpdateRequestSecret :: Maybe Text
    <*> arbitrary -- itemWebhookUpdateRequestAccessToken :: Text
    <*> arbitrary -- itemWebhookUpdateRequestWebhook :: Text
  
instance Arbitrary ItemWebhookUpdateResponse where
  arbitrary = sized genItemWebhookUpdateResponse

genItemWebhookUpdateResponse :: Int -> Gen ItemWebhookUpdateResponse
genItemWebhookUpdateResponse n =
  ItemWebhookUpdateResponse
    <$> arbitraryReduced n -- itemWebhookUpdateResponseItem :: Item
    <*> arbitrary -- itemWebhookUpdateResponseRequestId :: Text
  
instance Arbitrary JWKPublicKey where
  arbitrary = sized genJWKPublicKey

genJWKPublicKey :: Int -> Gen JWKPublicKey
genJWKPublicKey n =
  JWKPublicKey
    <$> arbitraryReducedMaybe n -- jWKPublicKeyAlg :: Maybe Text
    <*> arbitraryReducedMaybe n -- jWKPublicKeyCrv :: Maybe Text
    <*> arbitraryReducedMaybe n -- jWKPublicKeyKid :: Maybe Text
    <*> arbitraryReducedMaybe n -- jWKPublicKeyKty :: Maybe Text
    <*> arbitraryReducedMaybe n -- jWKPublicKeyUse :: Maybe Text
    <*> arbitraryReducedMaybe n -- jWKPublicKeyX :: Maybe Text
    <*> arbitraryReducedMaybe n -- jWKPublicKeyY :: Maybe Text
    <*> arbitraryReducedMaybe n -- jWKPublicKeyCreatedAt :: Maybe Int
    <*> arbitraryReducedMaybe n -- jWKPublicKeyExpiredAt :: Maybe Int
  
instance Arbitrary JWTHeader where
  arbitrary = sized genJWTHeader

genJWTHeader :: Int -> Gen JWTHeader
genJWTHeader n =
  JWTHeader
    <$> arbitrary -- jWTHeaderId :: Text
  
instance Arbitrary LiabilitiesGetRequest where
  arbitrary = sized genLiabilitiesGetRequest

genLiabilitiesGetRequest :: Int -> Gen LiabilitiesGetRequest
genLiabilitiesGetRequest n =
  LiabilitiesGetRequest
    <$> arbitraryReducedMaybe n -- liabilitiesGetRequestClientId :: Maybe Text
    <*> arbitraryReducedMaybe n -- liabilitiesGetRequestSecret :: Maybe Text
    <*> arbitrary -- liabilitiesGetRequestAccessToken :: Text
    <*> arbitraryReducedMaybe n -- liabilitiesGetRequestOptions :: Maybe LiabilitiesGetRequestOptions
  
instance Arbitrary LiabilitiesGetRequestOptions where
  arbitrary = sized genLiabilitiesGetRequestOptions

genLiabilitiesGetRequestOptions :: Int -> Gen LiabilitiesGetRequestOptions
genLiabilitiesGetRequestOptions n =
  LiabilitiesGetRequestOptions
    <$> arbitraryReducedMaybe n -- liabilitiesGetRequestOptionsAccountIds :: Maybe [Text]
  
instance Arbitrary LiabilitiesGetResponse where
  arbitrary = sized genLiabilitiesGetResponse

genLiabilitiesGetResponse :: Int -> Gen LiabilitiesGetResponse
genLiabilitiesGetResponse n =
  LiabilitiesGetResponse
    <$> arbitraryReduced n -- liabilitiesGetResponseAccounts :: [AccountBase]
    <*> arbitraryReduced n -- liabilitiesGetResponseItem :: Item
    <*> arbitraryReduced n -- liabilitiesGetResponseLiabilities :: LiabilitiesObject
    <*> arbitrary -- liabilitiesGetResponseRequestId :: Text
  
instance Arbitrary LiabilitiesObject where
  arbitrary = sized genLiabilitiesObject

genLiabilitiesObject :: Int -> Gen LiabilitiesObject
genLiabilitiesObject n =
  LiabilitiesObject
    <$> arbitraryReducedMaybe n -- liabilitiesObjectCredit :: Maybe [CreditCardLiability]
    <*> arbitraryReducedMaybe n -- liabilitiesObjectMortgage :: Maybe [MortgageLiability]
    <*> arbitraryReducedMaybe n -- liabilitiesObjectStudent :: Maybe [StudentLoan]
  
instance Arbitrary LiabilityOverride where
  arbitrary = sized genLiabilityOverride

genLiabilityOverride :: Int -> Gen LiabilityOverride
genLiabilityOverride n =
  LiabilityOverride
    <$> arbitrary -- liabilityOverrideType :: Text
    <*> arbitrary -- liabilityOverridePurchaseApr :: Double
    <*> arbitrary -- liabilityOverrideCashApr :: Double
    <*> arbitrary -- liabilityOverrideBalanceTransferApr :: Double
    <*> arbitrary -- liabilityOverrideSpecialApr :: Double
    <*> arbitrary -- liabilityOverrideLastPaymentAmount :: Double
    <*> arbitrary -- liabilityOverrideLastStatementBalance :: Double
    <*> arbitrary -- liabilityOverrideMinimumPaymentAmount :: Double
    <*> arbitrary -- liabilityOverrideIsOverdue :: Bool
    <*> arbitrary -- liabilityOverrideOriginationDate :: Text
    <*> arbitrary -- liabilityOverridePrincipal :: Double
    <*> arbitrary -- liabilityOverrideNominalApr :: Double
    <*> arbitrary -- liabilityOverrideInterestCapitalizationGracePeriodMonths :: Double
    <*> arbitraryReduced n -- liabilityOverrideRepaymentModel :: StudentLoanRepaymentModel
    <*> arbitrary -- liabilityOverrideExpectedPayoffDate :: Text
    <*> arbitrary -- liabilityOverrideGuarantor :: Text
    <*> arbitrary -- liabilityOverrideIsFederal :: Bool
    <*> arbitrary -- liabilityOverrideLoanName :: Text
    <*> arbitrary -- liabilityOverrideLoanStatus :: Text
    <*> arbitrary -- liabilityOverridePaymentReferenceNumber :: Text
    <*> arbitrary -- liabilityOverridePslfStatus :: Text
    <*> arbitrary -- liabilityOverrideRepaymentPlanDescription :: Text
    <*> arbitrary -- liabilityOverrideRepaymentPlanType :: Text
    <*> arbitrary -- liabilityOverrideSequenceNumber :: Text
    <*> arbitraryReduced n -- liabilityOverrideServicerAddress :: Address
  
instance Arbitrary LinkTokenAccountFilters where
  arbitrary = sized genLinkTokenAccountFilters

genLinkTokenAccountFilters :: Int -> Gen LinkTokenAccountFilters
genLinkTokenAccountFilters n =
  LinkTokenAccountFilters
    <$> arbitraryReducedMaybe n -- linkTokenAccountFiltersDepository :: Maybe DepositoryFilter
    <*> arbitraryReducedMaybe n -- linkTokenAccountFiltersCredit :: Maybe CreditFilter
    <*> arbitraryReducedMaybe n -- linkTokenAccountFiltersLoan :: Maybe LoanFilter
    <*> arbitraryReducedMaybe n -- linkTokenAccountFiltersInvestment :: Maybe InvestmentFilter
  
instance Arbitrary LinkTokenCreateRequestUpdateDict where
  arbitrary = LinkTokenCreateRequestUpdateDict <$> arbitrary <*> arbitrary
  
instance Arbitrary LinkTokenCreateRequestAuthOptions where
  arbitrary = sized genLinkTokenCreateRequestAuthOptions

genLinkTokenCreateRequestAuthOptions :: Int -> Gen LinkTokenCreateRequestAuthOptions
genLinkTokenCreateRequestAuthOptions n =
  LinkTokenCreateRequestAuthOptions
    <$> arbitraryReducedMaybe n -- authTypeSelectEnabled :: Maybe Bool
    <*> arbitraryReducedMaybe n -- automatedMicrodepositsEnabled :: Maybe Bool
    <*> arbitraryReducedMaybe n -- instantMatchEnabled :: Maybe Bool
    <*> arbitraryReducedMaybe n -- sameDayMicrodepositsEnabled :: Maybe Bool

instance Arbitrary LinkTokenCreateRequestTransactionsOptions where
  arbitrary = LinkTokenCreateRequestTransactionsOptions <$> arbitrary
  
instance Arbitrary LinkTokenCreateRequest where
  arbitrary = sized genLinkTokenCreateRequest

genLinkTokenCreateRequest :: Int -> Gen LinkTokenCreateRequest
genLinkTokenCreateRequest n =
  LinkTokenCreateRequest
    <$> arbitraryReducedMaybe n -- linkTokenCreateRequestClientId :: Maybe Text
    <*> arbitraryReducedMaybe n -- linkTokenCreateRequestSecret :: Maybe Text
    <*> arbitrary -- linkTokenCreateRequestClientName :: Text
    <*> arbitrary -- linkTokenCreateRequestLanguage :: Text
    <*> arbitraryReduced n -- linkTokenCreateRequestCountryCodes :: [CountryCode]
    <*> arbitraryReduced n -- linkTokenCreateRequestUser :: LinkTokenCreateRequestUser
    <*> arbitraryReducedMaybe n -- linkTokenCreateRequestProducts :: Maybe [Products]
    <*> arbitraryReducedMaybe n -- linkTokenCreateRequestRequiredIfSupportedProducts :: Maybe [RequiredIfSupportedProducts]
    <*> arbitraryReducedMaybe n -- linkTokenCreateRequestAdditionalConsentedProducts :: Maybe [AdditionalConsentedProducts]
    <*> arbitraryReducedMaybe n -- linkTokenCreateRequestWebhook :: Maybe Text
    <*> arbitraryReducedMaybe n -- linkTokenCreateRequestAccessToken :: Maybe Text
    <*> arbitraryReducedMaybe n -- linkTokenCreateRequestLinkCustomizationName :: Maybe Text
    <*> arbitraryReducedMaybe n -- linkTokenCreateRequestRedirectUri :: Maybe Text
    <*> arbitraryReducedMaybe n -- linkTokenCreateRequestAndroidPackageName :: Maybe Text
    <*> arbitraryReducedMaybe n -- linkTokenCreateRequestAccountFilters :: Maybe LinkTokenAccountFilters
    <*> arbitraryReducedMaybe n -- linkTokenCreateRequestInstitutionId :: Maybe Text
    <*> arbitraryReducedMaybe n -- linkTokenCreateRequestPaymentInitiation :: Maybe LinkTokenCreateRequestPaymentInitiation
    <*> arbitraryReducedMaybe n -- linkTokenCreateRequestDepositSwitch :: Maybe LinkTokenCreateRequestDepositSwitch
    <*> arbitraryReducedMaybe n -- linkTokenCreateRequestUpdate :: Maybe LinkTokenCreateRequestUpdateDict
    <*> arbitraryReducedMaybe n -- linkTokenCreateRequestAuth :: Maybe LinkTokenCreateRequestAuthOptions
    <*> arbitraryReducedMaybe n -- linkTokenCreateRequestTransactions :: Maybe LinkTokenCreateRequestTransactionsOptions
  
instance Arbitrary LinkTokenCreateRequestAccountSubtypes where
  arbitrary = sized genLinkTokenCreateRequestAccountSubtypes

genLinkTokenCreateRequestAccountSubtypes :: Int -> Gen LinkTokenCreateRequestAccountSubtypes
genLinkTokenCreateRequestAccountSubtypes n =
  LinkTokenCreateRequestAccountSubtypes
    <$> arbitraryReducedMaybe n -- linkTokenCreateRequestAccountSubtypesDepository :: Maybe (Map.Map String A.Value)
    <*> arbitraryReducedMaybe n -- linkTokenCreateRequestAccountSubtypesCredit :: Maybe (Map.Map String A.Value)
    <*> arbitraryReducedMaybe n -- linkTokenCreateRequestAccountSubtypesLoan :: Maybe (Map.Map String A.Value)
    <*> arbitraryReducedMaybe n -- linkTokenCreateRequestAccountSubtypesInvestment :: Maybe (Map.Map String A.Value)
  
instance Arbitrary LinkTokenCreateRequestDepositSwitch where
  arbitrary = sized genLinkTokenCreateRequestDepositSwitch

genLinkTokenCreateRequestDepositSwitch :: Int -> Gen LinkTokenCreateRequestDepositSwitch
genLinkTokenCreateRequestDepositSwitch n =
  LinkTokenCreateRequestDepositSwitch
    <$> arbitrary -- linkTokenCreateRequestDepositSwitchDepositSwitchId :: Text
  
instance Arbitrary LinkTokenCreateRequestIncomeVerification where
  arbitrary = sized genLinkTokenCreateRequestIncomeVerification

genLinkTokenCreateRequestIncomeVerification :: Int -> Gen LinkTokenCreateRequestIncomeVerification
genLinkTokenCreateRequestIncomeVerification n =
  LinkTokenCreateRequestIncomeVerification
    <$> arbitrary -- linkTokenCreateRequestIncomeVerificationIncomeVerificationId :: Text
    <*> arbitraryReducedMaybe n -- linkTokenCreateRequestIncomeVerificationAssetReportId :: Maybe Text
  
instance Arbitrary LinkTokenCreateRequestPaymentInitiation where
  arbitrary = sized genLinkTokenCreateRequestPaymentInitiation

genLinkTokenCreateRequestPaymentInitiation :: Int -> Gen LinkTokenCreateRequestPaymentInitiation
genLinkTokenCreateRequestPaymentInitiation n =
  LinkTokenCreateRequestPaymentInitiation
    <$> arbitrary -- linkTokenCreateRequestPaymentInitiationPaymentId :: Text
  
instance Arbitrary LinkTokenCreateRequestUser where
  arbitrary = sized genLinkTokenCreateRequestUser

genLinkTokenCreateRequestUser :: Int -> Gen LinkTokenCreateRequestUser
genLinkTokenCreateRequestUser n =
  LinkTokenCreateRequestUser
    <$> arbitrary -- linkTokenCreateRequestUserClientUserId :: Text
    <*> arbitraryReducedMaybe n -- linkTokenCreateRequestUserLegalName :: Maybe Text
    <*> arbitraryReducedMaybe n -- linkTokenCreateRequestUserPhoneNumber :: Maybe Text
    <*> arbitraryReducedMaybe n -- linkTokenCreateRequestUserPhoneNumberVerifiedTime :: Maybe Text
    <*> arbitraryReducedMaybe n -- linkTokenCreateRequestUserEmailAddress :: Maybe Text
    <*> arbitraryReducedMaybe n -- linkTokenCreateRequestUserEmailAddressVerifiedTime :: Maybe Text
    <*> arbitraryReducedMaybe n -- linkTokenCreateRequestUserSsn :: Maybe Text
    <*> arbitraryReducedMaybe n -- linkTokenCreateRequestUserDateOfBirth :: Maybe Text
  
instance Arbitrary LinkTokenCreateResponse where
  arbitrary = sized genLinkTokenCreateResponse

genLinkTokenCreateResponse :: Int -> Gen LinkTokenCreateResponse
genLinkTokenCreateResponse n =
  LinkTokenCreateResponse
    <$> arbitrary -- linkTokenCreateResponseLinkToken :: Text
    <*> arbitraryReduced n -- linkTokenCreateResponseExpiration :: DateTime
    <*> arbitrary -- linkTokenCreateResponseRequestId :: Text
  
instance Arbitrary LinkTokenGetMetadataResponse where
  arbitrary = sized genLinkTokenGetMetadataResponse

genLinkTokenGetMetadataResponse :: Int -> Gen LinkTokenGetMetadataResponse
genLinkTokenGetMetadataResponse n =
  LinkTokenGetMetadataResponse
    <$> arbitraryReducedMaybe n -- linkTokenGetMetadataResponseInitialProducts :: Maybe [Products]
    <*> arbitraryReducedMaybe n -- linkTokenGetMetadataResponseWebhook :: Maybe Text
    <*> arbitraryReducedMaybe n -- linkTokenGetMetadataResponseCountryCodes :: Maybe [CountryCode]
    <*> arbitraryReducedMaybe n -- linkTokenGetMetadataResponseLanguage :: Maybe Text
    <*> arbitraryReducedMaybe n -- linkTokenGetMetadataResponseAccountFilters :: Maybe AccountFiltersResponse
    <*> arbitraryReducedMaybe n -- linkTokenGetMetadataResponseRedirectUri :: Maybe Text
    <*> arbitraryReducedMaybe n -- linkTokenGetMetadataResponseClientName :: Maybe Text
  
instance Arbitrary LinkTokenGetRequest where
  arbitrary = sized genLinkTokenGetRequest

genLinkTokenGetRequest :: Int -> Gen LinkTokenGetRequest
genLinkTokenGetRequest n =
  LinkTokenGetRequest
    <$> arbitraryReducedMaybe n -- linkTokenGetRequestClientId :: Maybe Text
    <*> arbitraryReducedMaybe n -- linkTokenGetRequestSecret :: Maybe Text
    <*> arbitrary -- linkTokenGetRequestLinkToken :: Text
  
instance Arbitrary LinkTokenGetResponse where
  arbitrary = sized genLinkTokenGetResponse

genLinkTokenGetResponse :: Int -> Gen LinkTokenGetResponse
genLinkTokenGetResponse n =
  LinkTokenGetResponse
    <$> arbitraryReducedMaybe n -- linkTokenGetResponseLinkToken :: Maybe Text
    <*> arbitraryReducedMaybe n -- linkTokenGetResponseCreatedAt :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- linkTokenGetResponseExpiration :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- linkTokenGetResponseMetadata :: Maybe LinkTokenGetMetadataResponse
    <*> arbitrary -- linkTokenGetResponseRequestId :: Text
  
instance Arbitrary LoanFilter where
  arbitrary = sized genLoanFilter

genLoanFilter :: Int -> Gen LoanFilter
genLoanFilter n =
  LoanFilter
    <$> arbitraryReduced n -- loanFilterAccountSubtypes :: [AccountSubtype]
  
instance Arbitrary Location where
  arbitrary = sized genLocation

genLocation :: Int -> Gen Location
genLocation n =
  Location
    <$> arbitraryReducedMaybe n -- locationAddress :: Maybe Text
    <*> arbitraryReducedMaybe n -- locationCity :: Maybe Text
    <*> arbitraryReducedMaybe n -- locationRegion :: Maybe Text
    <*> arbitraryReducedMaybe n -- locationPostalCode :: Maybe Text
    <*> arbitraryReducedMaybe n -- locationCountry :: Maybe Text
    <*> arbitraryReducedMaybe n -- locationLat :: Maybe Double
    <*> arbitraryReducedMaybe n -- locationLon :: Maybe Double
    <*> arbitraryReducedMaybe n -- locationStoreNumber :: Maybe Text
  
instance Arbitrary MFA where
  arbitrary = sized genMFA

genMFA :: Int -> Gen MFA
genMFA n =
  MFA
    <$> arbitrary -- mFAType :: Text
    <*> arbitrary -- mFAQuestionRounds :: Double
    <*> arbitrary -- mFAQuestionsPerRound :: Double
    <*> arbitrary -- mFASelectionRounds :: Double
    <*> arbitrary -- mFASelectionsPerQuestion :: Double
  
instance Arbitrary Meta where
  arbitrary = sized genMeta

genMeta :: Int -> Gen Meta
genMeta n =
  Meta
    <$> arbitrary -- metaName :: Text
    <*> arbitrary -- metaOfficialName :: Text
    <*> arbitrary -- metaLimit :: Double
  
instance Arbitrary MortgageInterestRate where
  arbitrary = sized genMortgageInterestRate

genMortgageInterestRate :: Int -> Gen MortgageInterestRate
genMortgageInterestRate n =
  MortgageInterestRate
    <$> arbitraryReducedMaybe n -- mortgageInterestRatePercentage :: Maybe Double
    <*> arbitraryReducedMaybe n -- mortgageInterestRateType :: Maybe Text
  
instance Arbitrary MortgageLiability where
  arbitrary = sized genMortgageLiability

genMortgageLiability :: Int -> Gen MortgageLiability
genMortgageLiability n =
  MortgageLiability
    <$> arbitraryReducedMaybe n -- mortgageLiabilityAccountId :: Maybe Text
    <*> arbitrary -- mortgageLiabilityAccountNumber :: Text
    <*> arbitraryReducedMaybe n -- mortgageLiabilityCurrentLateFee :: Maybe Double
    <*> arbitraryReducedMaybe n -- mortgageLiabilityEscrowBalance :: Maybe Double
    <*> arbitraryReducedMaybe n -- mortgageLiabilityHasPmi :: Maybe Bool
    <*> arbitraryReducedMaybe n -- mortgageLiabilityHasPrepaymentPenalty :: Maybe Bool
    <*> arbitraryReducedMaybe n -- mortgageLiabilityInterestRate :: Maybe MortgageInterestRate
    <*> arbitraryReducedMaybe n -- mortgageLiabilityLastPaymentAmount :: Maybe Double
    <*> arbitraryReducedMaybe n -- mortgageLiabilityLastPaymentDate :: Maybe Text
    <*> arbitraryReducedMaybe n -- mortgageLiabilityLoanTypeDescription :: Maybe Text
    <*> arbitraryReducedMaybe n -- mortgageLiabilityLoanTerm :: Maybe Text
    <*> arbitraryReducedMaybe n -- mortgageLiabilityMaturityDate :: Maybe Text
    <*> arbitraryReducedMaybe n -- mortgageLiabilityNextMonthlyPayment :: Maybe Double
    <*> arbitraryReducedMaybe n -- mortgageLiabilityNextPaymentDueDate :: Maybe Text
    <*> arbitraryReducedMaybe n -- mortgageLiabilityOriginationDate :: Maybe Text
    <*> arbitraryReducedMaybe n -- mortgageLiabilityOriginationPrincipalAmount :: Maybe Double
    <*> arbitraryReducedMaybe n -- mortgageLiabilityPastDueAmount :: Maybe Double
    <*> arbitraryReducedMaybe n -- mortgageLiabilityPropertyAddress :: Maybe MortgagePropertyAddress
    <*> arbitraryReducedMaybe n -- mortgageLiabilityYtdInterestPaid :: Maybe Double
    <*> arbitraryReducedMaybe n -- mortgageLiabilityYtdPrincipalPaid :: Maybe Double
  
instance Arbitrary MortgagePropertyAddress where
  arbitrary = sized genMortgagePropertyAddress

genMortgagePropertyAddress :: Int -> Gen MortgagePropertyAddress
genMortgagePropertyAddress n =
  MortgagePropertyAddress
    <$> arbitraryReducedMaybe n -- mortgagePropertyAddressCity :: Maybe Text
    <*> arbitraryReducedMaybe n -- mortgagePropertyAddressCountry :: Maybe Text
    <*> arbitraryReducedMaybe n -- mortgagePropertyAddressPostalCode :: Maybe Text
    <*> arbitraryReducedMaybe n -- mortgagePropertyAddressRegion :: Maybe Text
    <*> arbitraryReducedMaybe n -- mortgagePropertyAddressStreet :: Maybe Text
  
instance Arbitrary NullableAccessToken where
  arbitrary = sized genNullableAccessToken

genNullableAccessToken :: Int -> Gen NullableAccessToken
genNullableAccessToken n =
  
  pure NullableAccessToken
   
instance Arbitrary NullableAddress where
  arbitrary = sized genNullableAddress

genNullableAddress :: Int -> Gen NullableAddress
genNullableAddress n =
  NullableAddress
    <$> arbitraryReduced n -- nullableAddressData :: AddressData
    <*> arbitraryReducedMaybe n -- nullableAddressPrimary :: Maybe Bool
  
instance Arbitrary NullableAddressData where
  arbitrary = sized genNullableAddressData

genNullableAddressData :: Int -> Gen NullableAddressData
genNullableAddressData n =
  NullableAddressData
    <$> arbitrary -- nullableAddressDataCity :: Text
    <*> arbitraryReducedMaybe n -- nullableAddressDataRegion :: Maybe Text
    <*> arbitrary -- nullableAddressDataStreet :: Text
    <*> arbitraryReducedMaybe n -- nullableAddressDataPostalCode :: Maybe Text
    <*> arbitrary -- nullableAddressDataCountry :: Text
  
instance Arbitrary NullableItemStatus where
  arbitrary = sized genNullableItemStatus

genNullableItemStatus :: Int -> Gen NullableItemStatus
genNullableItemStatus n =
  NullableItemStatus
    <$> arbitraryReducedMaybe n -- nullableItemStatusInvestments :: Maybe (Map.Map String A.Value)
    <*> arbitraryReducedMaybe n -- nullableItemStatusTransactions :: Maybe (Map.Map String A.Value)
    <*> arbitraryReducedMaybe n -- nullableItemStatusLastWebhook :: Maybe (Map.Map String A.Value)
  
instance Arbitrary NullableNumbersACH where
  arbitrary = sized genNullableNumbersACH

genNullableNumbersACH :: Int -> Gen NullableNumbersACH
genNullableNumbersACH n =
  NullableNumbersACH
    <$> arbitrary -- nullableNumbersACHAccountId :: Text
    <*> arbitrary -- nullableNumbersACHAccount :: Text
    <*> arbitrary -- nullableNumbersACHRouting :: Text
    <*> arbitraryReducedMaybe n -- nullableNumbersACHWireRouting :: Maybe Text
  
instance Arbitrary NullableNumbersBACS where
  arbitrary = sized genNullableNumbersBACS

genNullableNumbersBACS :: Int -> Gen NullableNumbersBACS
genNullableNumbersBACS n =
  NullableNumbersBACS
    <$> arbitrary -- nullableNumbersBACSAccountId :: Text
    <*> arbitrary -- nullableNumbersBACSAccount :: Text
    <*> arbitrary -- nullableNumbersBACSSortCode :: Text
  
instance Arbitrary NullableNumbersEFT where
  arbitrary = sized genNullableNumbersEFT

genNullableNumbersEFT :: Int -> Gen NullableNumbersEFT
genNullableNumbersEFT n =
  NullableNumbersEFT
    <$> arbitrary -- nullableNumbersEFTAccountId :: Text
    <*> arbitrary -- nullableNumbersEFTAccount :: Text
    <*> arbitrary -- nullableNumbersEFTInstitution :: Text
    <*> arbitrary -- nullableNumbersEFTBranch :: Text
  
instance Arbitrary NullableNumbersInternational where
  arbitrary = sized genNullableNumbersInternational

genNullableNumbersInternational :: Int -> Gen NullableNumbersInternational
genNullableNumbersInternational n =
  NullableNumbersInternational
    <$> arbitrary -- nullableNumbersInternationalAccountId :: Text
    <*> arbitrary -- nullableNumbersInternationalIban :: Text
    <*> arbitrary -- nullableNumbersInternationalBic :: Text
  
instance Arbitrary NullableRecipientBACS where
  arbitrary = sized genNullableRecipientBACS

genNullableRecipientBACS :: Int -> Gen NullableRecipientBACS
genNullableRecipientBACS n =
  NullableRecipientBACS
    <$> arbitraryReducedMaybe n -- nullableRecipientBACSAccount :: Maybe Text
    <*> arbitraryReducedMaybe n -- nullableRecipientBACSSortCode :: Maybe Text
  
instance Arbitrary Numbers where
  arbitrary = sized genNumbers

genNumbers :: Int -> Gen Numbers
genNumbers n =
  Numbers
    <$> arbitrary -- numbersAccount :: Text
    <*> arbitrary -- numbersAchRouting :: Text
    <*> arbitrary -- numbersAchWireRouting :: Text
    <*> arbitrary -- numbersEftInstitution :: Text
    <*> arbitrary -- numbersEftBranch :: Text
    <*> arbitrary -- numbersInternationalBic :: Text
    <*> arbitrary -- numbersInternationalIban :: Text
    <*> arbitrary -- numbersBacsSortCode :: Text
  
instance Arbitrary NumbersACH where
  arbitrary = sized genNumbersACH

genNumbersACH :: Int -> Gen NumbersACH
genNumbersACH n =
  NumbersACH
    <$> arbitrary -- numbersACHAccountId :: Text
    <*> arbitrary -- numbersACHAccount :: Text
    <*> arbitrary -- numbersACHRouting :: Text
    <*> arbitraryReducedMaybe n -- numbersACHWireRouting :: Maybe Text
  
instance Arbitrary NumbersBACS where
  arbitrary = sized genNumbersBACS

genNumbersBACS :: Int -> Gen NumbersBACS
genNumbersBACS n =
  NumbersBACS
    <$> arbitrary -- numbersBACSAccountId :: Text
    <*> arbitrary -- numbersBACSAccount :: Text
    <*> arbitrary -- numbersBACSSortCode :: Text
  
instance Arbitrary NumbersEFT where
  arbitrary = sized genNumbersEFT

genNumbersEFT :: Int -> Gen NumbersEFT
genNumbersEFT n =
  NumbersEFT
    <$> arbitrary -- numbersEFTAccountId :: Text
    <*> arbitrary -- numbersEFTAccount :: Text
    <*> arbitrary -- numbersEFTInstitution :: Text
    <*> arbitrary -- numbersEFTBranch :: Text
  
instance Arbitrary NumbersInternationals where
  arbitrary = sized genNumbersInternational

genNumbersInternational :: Int -> Gen NumbersInternationals
genNumbersInternational n =
  NumbersInternationals
    <$> arbitrary -- numbersInternationalAccountId :: Text
    <*> arbitrary -- numbersInternationalIban :: Text
    <*> arbitrary -- numbersInternationalBic :: Text
  
instance Arbitrary OverrideAccounts where
  arbitrary = sized genOverrideAccounts

genOverrideAccounts :: Int -> Gen OverrideAccounts
genOverrideAccounts n =
  OverrideAccounts
    <$> arbitraryReduced n -- overrideAccountsType :: AccountType
    <*> arbitraryReduced n -- overrideAccountsSubtype :: AccountSubtype
    <*> arbitrary -- overrideAccountsStartingBalance :: Double
    <*> arbitrary -- overrideAccountsForceAvailableBalance :: Double
    <*> arbitrary -- overrideAccountsCurrency :: Text
    <*> arbitraryReduced n -- overrideAccountsMeta :: Meta
    <*> arbitraryReduced n -- overrideAccountsNumbers :: Numbers
    <*> arbitraryReduced n -- overrideAccountsTransactions :: [TransactionOverride]
    <*> arbitraryReduced n -- overrideAccountsIdentity :: OwnerOverride
    <*> arbitraryReduced n -- overrideAccountsLiability :: LiabilityOverride
    <*> arbitraryReduced n -- overrideAccountsInflowModel :: InflowModel
  
instance Arbitrary Owner where
  arbitrary = sized genOwner

genOwner :: Int -> Gen Owner
genOwner n =
  Owner
    <$> arbitrary -- ownerNames :: [Text]
    <*> arbitraryReduced n -- ownerPhoneNumbers :: [PhoneNumber]
    <*> arbitraryReduced n -- ownerEmails :: [Email]
    <*> arbitraryReduced n -- ownerAddresses :: [Address]
  
instance Arbitrary OwnerOverride where
  arbitrary = sized genOwnerOverride

genOwnerOverride :: Int -> Gen OwnerOverride
genOwnerOverride n =
  OwnerOverride
    <$> arbitrary -- ownerOverrideNames :: [Text]
    <*> arbitraryReduced n -- ownerOverridePhoneNumbers :: [PhoneNumber]
    <*> arbitraryReduced n -- ownerOverrideEmails :: [Email]
    <*> arbitraryReduced n -- ownerOverrideAddresses :: [Address]
  
instance Arbitrary PSLFStatus where
  arbitrary = sized genPSLFStatus

genPSLFStatus :: Int -> Gen PSLFStatus
genPSLFStatus n =
  PSLFStatus
    <$> arbitraryReducedMaybe n -- pSLFStatusEstimatedEligibilityDate :: Maybe Text
    <*> arbitraryReducedMaybe n -- pSLFStatusPaymentsMade :: Maybe Double
    <*> arbitraryReducedMaybe n -- pSLFStatusPaymentsRemaining :: Maybe Double
  
instance Arbitrary PayFrequency where
  arbitrary = sized genPayFrequency

genPayFrequency :: Int -> Gen PayFrequency
genPayFrequency n =
  PayFrequency
    <$> arbitrary -- payFrequencyValue :: E'Value
    <*> arbitraryReduced n -- payFrequencyVerificationStatus :: VerificationStatus
  
instance Arbitrary PayPeriodDetails where
  arbitrary = sized genPayPeriodDetails

genPayPeriodDetails :: Int -> Gen PayPeriodDetails
genPayPeriodDetails n =
  PayPeriodDetails
    <$> arbitraryReducedMaybe n -- payPeriodDetailsStartDate :: Maybe Text
    <*> arbitraryReducedMaybe n -- payPeriodDetailsEndDate :: Maybe Text
    <*> arbitraryReducedMaybe n -- payPeriodDetailsPayDay :: Maybe Text
    <*> arbitraryReducedMaybe n -- payPeriodDetailsGrossEarnings :: Maybe Double
    <*> arbitraryReducedMaybe n -- payPeriodDetailsCheckAmount :: Maybe Double
  
instance Arbitrary PaymentAmount where
  arbitrary = sized genPaymentAmount

genPaymentAmount :: Int -> Gen PaymentAmount
genPaymentAmount n =
  PaymentAmount
    <$> arbitrary -- paymentAmountCurrency :: Text
    <*> arbitrary -- paymentAmountValue :: Double
  
instance Arbitrary PaymentInitiationAddress where
  arbitrary = sized genPaymentInitiationAddress

genPaymentInitiationAddress :: Int -> Gen PaymentInitiationAddress
genPaymentInitiationAddress n =
  PaymentInitiationAddress
    <$> arbitraryReducedMaybe n -- paymentInitiationAddressStreet :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- paymentInitiationAddressCity :: Maybe Text
    <*> arbitraryReducedMaybe n -- paymentInitiationAddressPostalCode :: Maybe Text
    <*> arbitraryReducedMaybe n -- paymentInitiationAddressCountry :: Maybe Text
  
instance Arbitrary PaymentInitiationPaymentCreateRequest where
  arbitrary = sized genPaymentInitiationPaymentCreateRequest

genPaymentInitiationPaymentCreateRequest :: Int -> Gen PaymentInitiationPaymentCreateRequest
genPaymentInitiationPaymentCreateRequest n =
  PaymentInitiationPaymentCreateRequest
    <$> arbitraryReducedMaybe n -- paymentInitiationPaymentCreateRequestClientId :: Maybe Text
    <*> arbitraryReducedMaybe n -- paymentInitiationPaymentCreateRequestSecret :: Maybe Text
    <*> arbitrary -- paymentInitiationPaymentCreateRequestRecipientId :: Text
    <*> arbitrary -- paymentInitiationPaymentCreateRequestReference :: Text
    <*> arbitraryReduced n -- paymentInitiationPaymentCreateRequestAmount :: Amount
    <*> arbitraryReducedMaybe n -- paymentInitiationPaymentCreateRequestSchedule :: Maybe ExternalPaymentSchedule
  
instance Arbitrary PaymentInitiationPaymentCreateResponse where
  arbitrary = sized genPaymentInitiationPaymentCreateResponse

genPaymentInitiationPaymentCreateResponse :: Int -> Gen PaymentInitiationPaymentCreateResponse
genPaymentInitiationPaymentCreateResponse n =
  PaymentInitiationPaymentCreateResponse
    <$> arbitrary -- paymentInitiationPaymentCreateResponsePaymentId :: Text
    <*> arbitrary -- paymentInitiationPaymentCreateResponseStatus :: Text
    <*> arbitrary -- paymentInitiationPaymentCreateResponseRequestId :: Text
  
instance Arbitrary PaymentInitiationPaymentGetRequest where
  arbitrary = sized genPaymentInitiationPaymentGetRequest

genPaymentInitiationPaymentGetRequest :: Int -> Gen PaymentInitiationPaymentGetRequest
genPaymentInitiationPaymentGetRequest n =
  PaymentInitiationPaymentGetRequest
    <$> arbitraryReducedMaybe n -- paymentInitiationPaymentGetRequestClientId :: Maybe Text
    <*> arbitraryReducedMaybe n -- paymentInitiationPaymentGetRequestSecret :: Maybe Text
    <*> arbitrary -- paymentInitiationPaymentGetRequestPaymentId :: Text
  
instance Arbitrary PaymentInitiationPaymentGetResponse where
  arbitrary = sized genPaymentInitiationPaymentGetResponse

genPaymentInitiationPaymentGetResponse :: Int -> Gen PaymentInitiationPaymentGetResponse
genPaymentInitiationPaymentGetResponse n =
  PaymentInitiationPaymentGetResponse
    <$> arbitrary -- paymentInitiationPaymentGetResponsePaymentId :: Text
    <*> arbitraryReducedMaybe n -- paymentInitiationPaymentGetResponseRequestId :: Maybe Text
    <*> arbitraryReduced n -- paymentInitiationPaymentGetResponseAmount :: PaymentAmount
    <*> arbitrary -- paymentInitiationPaymentGetResponseStatus :: E'Status
    <*> arbitrary -- paymentInitiationPaymentGetResponseRecipientId :: Text
    <*> arbitrary -- paymentInitiationPaymentGetResponseReference :: Text
    <*> arbitraryReducedMaybe n -- paymentInitiationPaymentGetResponseAdjustedReference :: Maybe Text
    <*> arbitrary -- paymentInitiationPaymentGetResponseLastStatusUpdate :: Text
    <*> arbitraryReducedMaybe n -- paymentInitiationPaymentGetResponseSchedule :: Maybe ExternalPaymentScheduleGet
  
instance Arbitrary PaymentInitiationPaymentListRequest where
  arbitrary = sized genPaymentInitiationPaymentListRequest

genPaymentInitiationPaymentListRequest :: Int -> Gen PaymentInitiationPaymentListRequest
genPaymentInitiationPaymentListRequest n =
  PaymentInitiationPaymentListRequest
    <$> arbitraryReducedMaybe n -- paymentInitiationPaymentListRequestClientId :: Maybe Text
    <*> arbitraryReducedMaybe n -- paymentInitiationPaymentListRequestSecret :: Maybe Text
    <*> arbitraryReducedMaybe n -- paymentInitiationPaymentListRequestCount :: Maybe Int
    <*> arbitraryReducedMaybe n -- paymentInitiationPaymentListRequestCursor :: Maybe Text
  
instance Arbitrary PaymentInitiationPaymentListResponse where
  arbitrary = sized genPaymentInitiationPaymentListResponse

genPaymentInitiationPaymentListResponse :: Int -> Gen PaymentInitiationPaymentListResponse
genPaymentInitiationPaymentListResponse n =
  PaymentInitiationPaymentListResponse
    <$> arbitraryReduced n -- paymentInitiationPaymentListResponsePayments :: [PaymentInitiationPaymentGetResponse]
    <*> arbitrary -- paymentInitiationPaymentListResponseNextCursor :: Text
    <*> arbitrary -- paymentInitiationPaymentListResponseRequestId :: Text
  
instance Arbitrary PaymentInitiationPaymentTokenCreateRequest where
  arbitrary = sized genPaymentInitiationPaymentTokenCreateRequest

genPaymentInitiationPaymentTokenCreateRequest :: Int -> Gen PaymentInitiationPaymentTokenCreateRequest
genPaymentInitiationPaymentTokenCreateRequest n =
  PaymentInitiationPaymentTokenCreateRequest
    <$> arbitraryReducedMaybe n -- paymentInitiationPaymentTokenCreateRequestClientId :: Maybe Text
    <*> arbitraryReducedMaybe n -- paymentInitiationPaymentTokenCreateRequestSecret :: Maybe Text
    <*> arbitrary -- paymentInitiationPaymentTokenCreateRequestPaymentId :: Text
  
instance Arbitrary PaymentInitiationPaymentTokenCreateResponse where
  arbitrary = sized genPaymentInitiationPaymentTokenCreateResponse

genPaymentInitiationPaymentTokenCreateResponse :: Int -> Gen PaymentInitiationPaymentTokenCreateResponse
genPaymentInitiationPaymentTokenCreateResponse n =
  PaymentInitiationPaymentTokenCreateResponse
    <$> arbitrary -- paymentInitiationPaymentTokenCreateResponsePaymentToken :: Text
    <*> arbitrary -- paymentInitiationPaymentTokenCreateResponsePaymentTokenExpirationTime :: Text
    <*> arbitrary -- paymentInitiationPaymentTokenCreateResponseRequestId :: Text
  
instance Arbitrary PaymentInitiationRecipient where
  arbitrary = sized genPaymentInitiationRecipient

genPaymentInitiationRecipient :: Int -> Gen PaymentInitiationRecipient
genPaymentInitiationRecipient n =
  PaymentInitiationRecipient
    <$> arbitrary -- paymentInitiationRecipientRecipientId :: Text
    <*> arbitrary -- paymentInitiationRecipientName :: Text
    <*> arbitraryReduced n -- paymentInitiationRecipientAddress :: PaymentInitiationAddress
    <*> arbitraryReducedMaybe n -- paymentInitiationRecipientIban :: Maybe Text
    <*> arbitraryReducedMaybe n -- paymentInitiationRecipientBacs :: Maybe (Map.Map String A.Value)
  
instance Arbitrary PaymentInitiationRecipientCreateRequest where
  arbitrary = sized genPaymentInitiationRecipientCreateRequest

genPaymentInitiationRecipientCreateRequest :: Int -> Gen PaymentInitiationRecipientCreateRequest
genPaymentInitiationRecipientCreateRequest n =
  PaymentInitiationRecipientCreateRequest
    <$> arbitraryReducedMaybe n -- paymentInitiationRecipientCreateRequestClientId :: Maybe Text
    <*> arbitraryReducedMaybe n -- paymentInitiationRecipientCreateRequestSecret :: Maybe Text
    <*> arbitrary -- paymentInitiationRecipientCreateRequestName :: Text
    <*> arbitraryReducedMaybe n -- paymentInitiationRecipientCreateRequestIban :: Maybe Text
    <*> arbitraryReducedMaybe n -- paymentInitiationRecipientCreateRequestBacs :: Maybe NullableRecipientBACS
    <*> arbitraryReducedMaybe n -- paymentInitiationRecipientCreateRequestAddress :: Maybe PaymentInitiationAddress
  
instance Arbitrary PaymentInitiationRecipientCreateResponse where
  arbitrary = sized genPaymentInitiationRecipientCreateResponse

genPaymentInitiationRecipientCreateResponse :: Int -> Gen PaymentInitiationRecipientCreateResponse
genPaymentInitiationRecipientCreateResponse n =
  PaymentInitiationRecipientCreateResponse
    <$> arbitrary -- paymentInitiationRecipientCreateResponseRecipientId :: Text
    <*> arbitrary -- paymentInitiationRecipientCreateResponseRequestId :: Text
  
instance Arbitrary PaymentInitiationRecipientGetRequest where
  arbitrary = sized genPaymentInitiationRecipientGetRequest

genPaymentInitiationRecipientGetRequest :: Int -> Gen PaymentInitiationRecipientGetRequest
genPaymentInitiationRecipientGetRequest n =
  PaymentInitiationRecipientGetRequest
    <$> arbitraryReducedMaybe n -- paymentInitiationRecipientGetRequestClientId :: Maybe Text
    <*> arbitraryReducedMaybe n -- paymentInitiationRecipientGetRequestSecret :: Maybe Text
    <*> arbitrary -- paymentInitiationRecipientGetRequestRecipientId :: Text
  
instance Arbitrary PaymentInitiationRecipientGetResponse where
  arbitrary = sized genPaymentInitiationRecipientGetResponse

genPaymentInitiationRecipientGetResponse :: Int -> Gen PaymentInitiationRecipientGetResponse
genPaymentInitiationRecipientGetResponse n =
  PaymentInitiationRecipientGetResponse
    <$> arbitrary -- paymentInitiationRecipientGetResponseRecipientId :: Text
    <*> arbitrary -- paymentInitiationRecipientGetResponseName :: Text
    <*> arbitraryReducedMaybe n -- paymentInitiationRecipientGetResponseAddress :: Maybe PaymentInitiationAddress
    <*> arbitrary -- paymentInitiationRecipientGetResponseIban :: Text
    <*> arbitraryReducedMaybe n -- paymentInitiationRecipientGetResponseBacs :: Maybe NullableRecipientBACS
    <*> arbitrary -- paymentInitiationRecipientGetResponseRequestId :: Text
  
instance Arbitrary PaymentInitiationRecipientListRequest where
  arbitrary = sized genPaymentInitiationRecipientListRequest

genPaymentInitiationRecipientListRequest :: Int -> Gen PaymentInitiationRecipientListRequest
genPaymentInitiationRecipientListRequest n =
  PaymentInitiationRecipientListRequest
    <$> arbitraryReducedMaybe n -- paymentInitiationRecipientListRequestClientId :: Maybe Text
    <*> arbitraryReducedMaybe n -- paymentInitiationRecipientListRequestSecret :: Maybe Text
  
instance Arbitrary PaymentInitiationRecipientListResponse where
  arbitrary = sized genPaymentInitiationRecipientListResponse

genPaymentInitiationRecipientListResponse :: Int -> Gen PaymentInitiationRecipientListResponse
genPaymentInitiationRecipientListResponse n =
  PaymentInitiationRecipientListResponse
    <$> arbitraryReduced n -- paymentInitiationRecipientListResponseRecipients :: [PaymentInitiationRecipient]
    <*> arbitrary -- paymentInitiationRecipientListResponseRequestId :: Text
  
instance Arbitrary PaymentMeta where
  arbitrary = sized genPaymentMeta

genPaymentMeta :: Int -> Gen PaymentMeta
genPaymentMeta n =
  PaymentMeta
    <$> arbitraryReducedMaybe n -- paymentMetaReferenceNumber :: Maybe Text
    <*> arbitraryReducedMaybe n -- paymentMetaPpdId :: Maybe Text
    <*> arbitraryReducedMaybe n -- paymentMetaPayee :: Maybe Text
    <*> arbitraryReducedMaybe n -- paymentMetaByOrderOf :: Maybe Text
    <*> arbitraryReducedMaybe n -- paymentMetaPayer :: Maybe Text
    <*> arbitraryReducedMaybe n -- paymentMetaPaymentMethod :: Maybe Text
    <*> arbitraryReducedMaybe n -- paymentMetaPaymentProcessor :: Maybe Text
    <*> arbitraryReducedMaybe n -- paymentMetaReason :: Maybe Text
  
instance Arbitrary PaymentStatusUpdateWebhook where
  arbitrary = sized genPaymentStatusUpdateWebhook

genPaymentStatusUpdateWebhook :: Int -> Gen PaymentStatusUpdateWebhook
genPaymentStatusUpdateWebhook n =
  PaymentStatusUpdateWebhook
    <$> arbitrary -- paymentStatusUpdateWebhookWebhookType :: Text
    <*> arbitrary -- paymentStatusUpdateWebhookWebhookCode :: Text
    <*> arbitrary -- paymentStatusUpdateWebhookPaymentId :: Text
    <*> arbitrary -- paymentStatusUpdateWebhookNewPaymentStatus :: E'Status
    <*> arbitrary -- paymentStatusUpdateWebhookOldPaymentStatus :: E'Status
    <*> arbitraryReducedMaybe n -- paymentStatusUpdateWebhookOriginalReference :: Maybe Text
    <*> arbitraryReducedMaybe n -- paymentStatusUpdateWebhookAdjustedReference :: Maybe Text
    <*> arbitraryReducedMaybe n -- paymentStatusUpdateWebhookOriginalStartDate :: Maybe Date
    <*> arbitraryReducedMaybe n -- paymentStatusUpdateWebhookAdjustedStartDate :: Maybe Date
    <*> arbitrary -- paymentStatusUpdateWebhookTimestamp :: Text
    <*> arbitraryReducedMaybe n -- paymentStatusUpdateWebhookError :: Maybe Error
  
instance Arbitrary Paystub where
  arbitrary = sized genPaystub

genPaystub :: Int -> Gen Paystub
genPaystub n =
  Paystub
    <$> arbitrary -- paystubPaystubId :: Text
    <*> arbitraryReducedMaybe n -- paystubAccountId :: Maybe Text
    <*> arbitraryReduced n -- paystubEmployer :: Employer
    <*> arbitraryReduced n -- paystubEmployee :: Employee
    <*> arbitraryReduced n -- paystubPayPeriodDetails :: PayPeriodDetails
    <*> arbitraryReduced n -- paystubIncomeBreakdown :: IncomeBreakdown
    <*> arbitraryReduced n -- paystubYtdEarnings :: PaystubYTDDetails
  
instance Arbitrary PaystubDeduction where
  arbitrary = sized genPaystubDeduction

genPaystubDeduction :: Int -> Gen PaystubDeduction
genPaystubDeduction n =
  PaystubDeduction
    <$> arbitraryReducedMaybe n -- paystubDeductionType :: Maybe Text
    <*> arbitraryReducedMaybe n -- paystubDeductionIsPretax :: Maybe Bool
    <*> arbitraryReducedMaybe n -- paystubDeductionTotal :: Maybe Double
  
instance Arbitrary PaystubYTDDetails where
  arbitrary = sized genPaystubYTDDetails

genPaystubYTDDetails :: Int -> Gen PaystubYTDDetails
genPaystubYTDDetails n =
  PaystubYTDDetails
    <$> arbitrary -- paystubYTDDetailsGrossEarnings :: Double
    <*> arbitrary -- paystubYTDDetailsNetEarnings :: Double
  
instance Arbitrary PendingExpirationWebhook where
  arbitrary = sized genPendingExpirationWebhook

genPendingExpirationWebhook :: Int -> Gen PendingExpirationWebhook
genPendingExpirationWebhook n =
  PendingExpirationWebhook
    <$> arbitrary -- pendingExpirationWebhookWebhookType :: Text
    <*> arbitrary -- pendingExpirationWebhookWebhookCode :: Text
    <*> arbitrary -- pendingExpirationWebhookItemId :: Text
    <*> arbitrary -- pendingExpirationWebhookConsentExpirationTime :: Text
  
instance Arbitrary PhoneNumber where
  arbitrary = sized genPhoneNumber

genPhoneNumber :: Int -> Gen PhoneNumber
genPhoneNumber n =
  PhoneNumber
    <$> arbitrary -- phoneNumberData :: Text
    <*> arbitraryReducedMaybe n -- phoneNumberPrimary :: Maybe Bool
    <*> arbitraryReducedMaybe n -- phoneNumberType :: Maybe E'Type
  
instance Arbitrary ProcessorApexProcessorTokenCreateRequest where
  arbitrary = sized genProcessorApexProcessorTokenCreateRequest

genProcessorApexProcessorTokenCreateRequest :: Int -> Gen ProcessorApexProcessorTokenCreateRequest
genProcessorApexProcessorTokenCreateRequest n =
  ProcessorApexProcessorTokenCreateRequest
    <$> arbitraryReducedMaybe n -- processorApexProcessorTokenCreateRequestClientId :: Maybe Text
    <*> arbitraryReducedMaybe n -- processorApexProcessorTokenCreateRequestSecret :: Maybe Text
    <*> arbitrary -- processorApexProcessorTokenCreateRequestAccessToken :: Text
    <*> arbitrary -- processorApexProcessorTokenCreateRequestAccountId :: Text
  
instance Arbitrary ProcessorAuthGetRequest where
  arbitrary = sized genProcessorAuthGetRequest

genProcessorAuthGetRequest :: Int -> Gen ProcessorAuthGetRequest
genProcessorAuthGetRequest n =
  ProcessorAuthGetRequest
    <$> arbitraryReducedMaybe n -- processorAuthGetRequestClientId :: Maybe Text
    <*> arbitraryReducedMaybe n -- processorAuthGetRequestSecret :: Maybe Text
    <*> arbitrary -- processorAuthGetRequestProcessorToken :: Text
  
instance Arbitrary ProcessorAuthGetResponse where
  arbitrary = sized genProcessorAuthGetResponse

genProcessorAuthGetResponse :: Int -> Gen ProcessorAuthGetResponse
genProcessorAuthGetResponse n =
  ProcessorAuthGetResponse
    <$> arbitrary -- processorAuthGetResponseRequestId :: Text
    <*> arbitraryReduced n -- processorAuthGetResponseNumbers :: ProcessorNumber
    <*> arbitraryReduced n -- processorAuthGetResponseAccount :: AccountBase
  
instance Arbitrary ProcessorBalanceGetRequest where
  arbitrary = sized genProcessorBalanceGetRequest

genProcessorBalanceGetRequest :: Int -> Gen ProcessorBalanceGetRequest
genProcessorBalanceGetRequest n =
  ProcessorBalanceGetRequest
    <$> arbitraryReducedMaybe n -- processorBalanceGetRequestClientId :: Maybe Text
    <*> arbitraryReducedMaybe n -- processorBalanceGetRequestSecret :: Maybe Text
    <*> arbitrary -- processorBalanceGetRequestProcessorToken :: Text
  
instance Arbitrary ProcessorBalanceGetResponse where
  arbitrary = sized genProcessorBalanceGetResponse

genProcessorBalanceGetResponse :: Int -> Gen ProcessorBalanceGetResponse
genProcessorBalanceGetResponse n =
  ProcessorBalanceGetResponse
    <$> arbitraryReduced n -- processorBalanceGetResponseAccount :: AccountBase
    <*> arbitrary -- processorBalanceGetResponseRequestId :: Text
  
instance Arbitrary ProcessorIdentityGetRequest where
  arbitrary = sized genProcessorIdentityGetRequest

genProcessorIdentityGetRequest :: Int -> Gen ProcessorIdentityGetRequest
genProcessorIdentityGetRequest n =
  ProcessorIdentityGetRequest
    <$> arbitraryReducedMaybe n -- processorIdentityGetRequestClientId :: Maybe Text
    <*> arbitraryReducedMaybe n -- processorIdentityGetRequestSecret :: Maybe Text
    <*> arbitrary -- processorIdentityGetRequestProcessorToken :: Text
  
instance Arbitrary ProcessorIdentityGetResponse where
  arbitrary = sized genProcessorIdentityGetResponse

genProcessorIdentityGetResponse :: Int -> Gen ProcessorIdentityGetResponse
genProcessorIdentityGetResponse n =
  ProcessorIdentityGetResponse
    <$> arbitraryReduced n -- processorIdentityGetResponseAccount :: AccountIdentity
    <*> arbitrary -- processorIdentityGetResponseRequestId :: Text
  
instance Arbitrary ProcessorNumber where
  arbitrary = sized genProcessorNumber

genProcessorNumber :: Int -> Gen ProcessorNumber
genProcessorNumber n =
  ProcessorNumber
    <$> arbitraryReducedMaybe n -- processorNumberAch :: Maybe NullableNumbersACH
    <*> arbitraryReducedMaybe n -- processorNumberEft :: Maybe NullableNumbersEFT
    <*> arbitraryReducedMaybe n -- processorNumberInternational :: Maybe NullableNumbersInternational
    <*> arbitraryReducedMaybe n -- processorNumberBacs :: Maybe NullableNumbersBACS
  
instance Arbitrary ProcessorStripeBankAccountTokenCreateRequest where
  arbitrary = sized genProcessorStripeBankAccountTokenCreateRequest

genProcessorStripeBankAccountTokenCreateRequest :: Int -> Gen ProcessorStripeBankAccountTokenCreateRequest
genProcessorStripeBankAccountTokenCreateRequest n =
  ProcessorStripeBankAccountTokenCreateRequest
    <$> arbitraryReducedMaybe n -- processorStripeBankAccountTokenCreateRequestClientId :: Maybe Text
    <*> arbitraryReducedMaybe n -- processorStripeBankAccountTokenCreateRequestSecret :: Maybe Text
    <*> arbitrary -- processorStripeBankAccountTokenCreateRequestAccessToken :: Text
    <*> arbitrary -- processorStripeBankAccountTokenCreateRequestAccountId :: Text
  
instance Arbitrary ProcessorStripeBankAccountTokenCreateResponse where
  arbitrary = sized genProcessorStripeBankAccountTokenCreateResponse

genProcessorStripeBankAccountTokenCreateResponse :: Int -> Gen ProcessorStripeBankAccountTokenCreateResponse
genProcessorStripeBankAccountTokenCreateResponse n =
  ProcessorStripeBankAccountTokenCreateResponse
    <$> arbitrary -- processorStripeBankAccountTokenCreateResponseStripeBankAccountToken :: Text
    <*> arbitrary -- processorStripeBankAccountTokenCreateResponseRequestId :: Text
  
instance Arbitrary ProcessorTokenCreateRequest where
  arbitrary = sized genProcessorTokenCreateRequest

genProcessorTokenCreateRequest :: Int -> Gen ProcessorTokenCreateRequest
genProcessorTokenCreateRequest n =
  ProcessorTokenCreateRequest
    <$> arbitraryReducedMaybe n -- processorTokenCreateRequestClientId :: Maybe Text
    <*> arbitraryReducedMaybe n -- processorTokenCreateRequestSecret :: Maybe Text
    <*> arbitrary -- processorTokenCreateRequestAccessToken :: Text
    <*> arbitrary -- processorTokenCreateRequestAccountId :: Text
    <*> arbitrary -- processorTokenCreateRequestProcessor :: Text
  
instance Arbitrary ProcessorTokenCreateResponse where
  arbitrary = sized genProcessorTokenCreateResponse

genProcessorTokenCreateResponse :: Int -> Gen ProcessorTokenCreateResponse
genProcessorTokenCreateResponse n =
  ProcessorTokenCreateResponse
    <$> arbitrary -- processorTokenCreateResponseProcessorToken :: Text
    <*> arbitrary -- processorTokenCreateResponseRequestId :: Text
  
instance Arbitrary ProductStatus where
  arbitrary = sized genProductStatus

genProductStatus :: Int -> Gen ProductStatus
genProductStatus n =
  ProductStatus
    <$> arbitrary -- productStatusStatus :: E'Status2
    <*> arbitrary -- productStatusLastStatusChange :: Text
    <*> arbitraryReduced n -- productStatusBreakdown :: ProductStatusBreakdown
  
instance Arbitrary ProductStatusBreakdown where
  arbitrary = sized genProductStatusBreakdown

genProductStatusBreakdown :: Int -> Gen ProductStatusBreakdown
genProductStatusBreakdown n =
  ProductStatusBreakdown
    <$> arbitrary -- productStatusBreakdownSuccess :: Double
    <*> arbitrary -- productStatusBreakdownErrorPlaid :: Double
    <*> arbitrary -- productStatusBreakdownErrorInstitution :: Double
    <*> arbitraryReducedMaybe n -- productStatusBreakdownRefreshInterval :: Maybe E'RefreshInterval
  
instance Arbitrary ProjectedIncomeSummaryFieldNumber where
  arbitrary = sized genProjectedIncomeSummaryFieldNumber

genProjectedIncomeSummaryFieldNumber :: Int -> Gen ProjectedIncomeSummaryFieldNumber
genProjectedIncomeSummaryFieldNumber n =
  ProjectedIncomeSummaryFieldNumber
    <$> arbitrary -- projectedIncomeSummaryFieldNumberValue :: Double
    <*> arbitraryReduced n -- projectedIncomeSummaryFieldNumberVerificationStatus :: VerificationStatus
  
instance Arbitrary RecaptchaRequiredError where
  arbitrary = sized genRecaptchaRequiredError

genRecaptchaRequiredError :: Int -> Gen RecaptchaRequiredError
genRecaptchaRequiredError n =
  RecaptchaRequiredError
    <$> arbitrary -- recaptchaRequiredErrorErrorType :: Text
    <*> arbitrary -- recaptchaRequiredErrorErrorCode :: Text
    <*> arbitrary -- recaptchaRequiredErrorDisplayMessage :: Text
    <*> arbitrary -- recaptchaRequiredErrorHttpCode :: Text
    <*> arbitrary -- recaptchaRequiredErrorLinkUserExperience :: Text
    <*> arbitrary -- recaptchaRequiredErrorCommonCauses :: Text
    <*> arbitrary -- recaptchaRequiredErrorTroubleshootingSteps :: Text
  
instance Arbitrary RecipientBACS where
  arbitrary = sized genRecipientBACS

genRecipientBACS :: Int -> Gen RecipientBACS
genRecipientBACS n =
  RecipientBACS
    <$> arbitraryReducedMaybe n -- recipientBACSAccount :: Maybe Text
    <*> arbitraryReducedMaybe n -- recipientBACSSortCode :: Maybe Text
  
instance Arbitrary RemovedTransaction where
  arbitrary = sized genRemovedTransaction

genRemovedTransaction :: Int -> Gen RemovedTransaction
genRemovedTransaction n =
  RemovedTransaction
    <$> arbitraryReducedMaybe n -- removedTransactionTransactionId :: Maybe Text
  
instance Arbitrary SandboxBankTransferSimulateRequest where
  arbitrary = sized genSandboxBankTransferSimulateRequest

genSandboxBankTransferSimulateRequest :: Int -> Gen SandboxBankTransferSimulateRequest
genSandboxBankTransferSimulateRequest n =
  SandboxBankTransferSimulateRequest
    <$> arbitraryReducedMaybe n -- sandboxBankTransferSimulateRequestClientId :: Maybe Text
    <*> arbitraryReducedMaybe n -- sandboxBankTransferSimulateRequestSecret :: Maybe Text
    <*> arbitrary -- sandboxBankTransferSimulateRequestBankTransferId :: Text
    <*> arbitrary -- sandboxBankTransferSimulateRequestEventType :: Text
    <*> arbitraryReducedMaybe n -- sandboxBankTransferSimulateRequestFailureReason :: Maybe BankTransferFailure
  
instance Arbitrary SandboxBankTransferSimulateResponse where
  arbitrary = sized genSandboxBankTransferSimulateResponse

genSandboxBankTransferSimulateResponse :: Int -> Gen SandboxBankTransferSimulateResponse
genSandboxBankTransferSimulateResponse n =
  SandboxBankTransferSimulateResponse
    <$> arbitrary -- sandboxBankTransferSimulateResponseRequestId :: Text
  
instance Arbitrary SandboxItemFireWebhookRequest where
  arbitrary = sized genSandboxItemFireWebhookRequest

genSandboxItemFireWebhookRequest :: Int -> Gen SandboxItemFireWebhookRequest
genSandboxItemFireWebhookRequest n =
  SandboxItemFireWebhookRequest
    <$> arbitraryReducedMaybe n -- sandboxItemFireWebhookRequestClientId :: Maybe Text
    <*> arbitraryReducedMaybe n -- sandboxItemFireWebhookRequestSecret :: Maybe Text
    <*> arbitrary -- sandboxItemFireWebhookRequestAccessToken :: Text
    <*> arbitraryReducedMaybe n -- sandboxItemFireWebhookRequestWebhookCode :: Maybe E'WebhookCode
  
instance Arbitrary SandboxItemFireWebhookResponse where
  arbitrary = sized genSandboxItemFireWebhookResponse

genSandboxItemFireWebhookResponse :: Int -> Gen SandboxItemFireWebhookResponse
genSandboxItemFireWebhookResponse n =
  SandboxItemFireWebhookResponse
    <$> arbitrary -- sandboxItemFireWebhookResponseWebhookFired :: Bool
    <*> arbitrary -- sandboxItemFireWebhookResponseRequestId :: Text
  
instance Arbitrary SandboxItemResetLoginRequest where
  arbitrary = sized genSandboxItemResetLoginRequest

genSandboxItemResetLoginRequest :: Int -> Gen SandboxItemResetLoginRequest
genSandboxItemResetLoginRequest n =
  SandboxItemResetLoginRequest
    <$> arbitraryReducedMaybe n -- sandboxItemResetLoginRequestClientId :: Maybe Text
    <*> arbitraryReducedMaybe n -- sandboxItemResetLoginRequestSecret :: Maybe Text
    <*> arbitrary -- sandboxItemResetLoginRequestAccessToken :: Text
  
instance Arbitrary SandboxItemResetLoginResponse where
  arbitrary = sized genSandboxItemResetLoginResponse

genSandboxItemResetLoginResponse :: Int -> Gen SandboxItemResetLoginResponse
genSandboxItemResetLoginResponse n =
  SandboxItemResetLoginResponse
    <$> arbitrary -- sandboxItemResetLoginResponseResetLogin :: Bool
    <*> arbitrary -- sandboxItemResetLoginResponseRequestId :: Text
  
instance Arbitrary SandboxItemSetVerificationStatusRequest where
  arbitrary = sized genSandboxItemSetVerificationStatusRequest

genSandboxItemSetVerificationStatusRequest :: Int -> Gen SandboxItemSetVerificationStatusRequest
genSandboxItemSetVerificationStatusRequest n =
  SandboxItemSetVerificationStatusRequest
    <$> arbitraryReducedMaybe n -- sandboxItemSetVerificationStatusRequestClientId :: Maybe Text
    <*> arbitraryReducedMaybe n -- sandboxItemSetVerificationStatusRequestSecret :: Maybe Text
    <*> arbitrary -- sandboxItemSetVerificationStatusRequestAccessToken :: Text
    <*> arbitrary -- sandboxItemSetVerificationStatusRequestAccountId :: Text
    <*> arbitrary -- sandboxItemSetVerificationStatusRequestVerificationStatus :: E'VerificationStatus
  
instance Arbitrary SandboxItemSetVerificationStatusResponse where
  arbitrary = sized genSandboxItemSetVerificationStatusResponse

genSandboxItemSetVerificationStatusResponse :: Int -> Gen SandboxItemSetVerificationStatusResponse
genSandboxItemSetVerificationStatusResponse n =
  SandboxItemSetVerificationStatusResponse
    <$> arbitrary -- sandboxItemSetVerificationStatusResponseRequestId :: Text
  
instance Arbitrary SandboxProcessorTokenCreateRequest where
  arbitrary = sized genSandboxProcessorTokenCreateRequest

genSandboxProcessorTokenCreateRequest :: Int -> Gen SandboxProcessorTokenCreateRequest
genSandboxProcessorTokenCreateRequest n =
  SandboxProcessorTokenCreateRequest
    <$> arbitraryReducedMaybe n -- sandboxProcessorTokenCreateRequestClientId :: Maybe Text
    <*> arbitraryReducedMaybe n -- sandboxProcessorTokenCreateRequestSecret :: Maybe Text
    <*> arbitrary -- sandboxProcessorTokenCreateRequestInstitutionId :: Text
    <*> arbitraryReducedMaybe n -- sandboxProcessorTokenCreateRequestOptions :: Maybe SandboxProcessorTokenCreateRequestOptions
  
instance Arbitrary SandboxProcessorTokenCreateRequestOptions where
  arbitrary = sized genSandboxProcessorTokenCreateRequestOptions

genSandboxProcessorTokenCreateRequestOptions :: Int -> Gen SandboxProcessorTokenCreateRequestOptions
genSandboxProcessorTokenCreateRequestOptions n =
  SandboxProcessorTokenCreateRequestOptions
    <$> arbitraryReducedMaybe n -- sandboxProcessorTokenCreateRequestOptionsOverrideUsername :: Maybe Text
    <*> arbitraryReducedMaybe n -- sandboxProcessorTokenCreateRequestOptionsOverridePassword :: Maybe Text
  
instance Arbitrary SandboxProcessorTokenCreateResponse where
  arbitrary = sized genSandboxProcessorTokenCreateResponse

genSandboxProcessorTokenCreateResponse :: Int -> Gen SandboxProcessorTokenCreateResponse
genSandboxProcessorTokenCreateResponse n =
  SandboxProcessorTokenCreateResponse
    <$> arbitrary -- sandboxProcessorTokenCreateResponseProcessorToken :: Text
    <*> arbitrary -- sandboxProcessorTokenCreateResponseRequestId :: Text
  
instance Arbitrary SandboxPublicTokenCreateRequest where
  arbitrary = sized genSandboxPublicTokenCreateRequest

genSandboxPublicTokenCreateRequest :: Int -> Gen SandboxPublicTokenCreateRequest
genSandboxPublicTokenCreateRequest n =
  SandboxPublicTokenCreateRequest
    <$> arbitraryReducedMaybe n -- sandboxPublicTokenCreateRequestClientId :: Maybe Text
    <*> arbitraryReducedMaybe n -- sandboxPublicTokenCreateRequestSecret :: Maybe Text
    <*> arbitrary -- sandboxPublicTokenCreateRequestInstitutionId :: Text
    <*> arbitraryReduced n -- sandboxPublicTokenCreateRequestInitialProducts :: [Products]
    <*> arbitraryReducedMaybe n -- sandboxPublicTokenCreateRequestOptions :: Maybe SandboxPublicTokenCreateRequestOptions
  
instance Arbitrary SandboxPublicTokenCreateRequestOptions where
  arbitrary = sized genSandboxPublicTokenCreateRequestOptions

genSandboxPublicTokenCreateRequestOptions :: Int -> Gen SandboxPublicTokenCreateRequestOptions
genSandboxPublicTokenCreateRequestOptions n =
  SandboxPublicTokenCreateRequestOptions
    <$> arbitraryReducedMaybe n -- sandboxPublicTokenCreateRequestOptionsWebhook :: Maybe Text
    <*> arbitraryReducedMaybe n -- sandboxPublicTokenCreateRequestOptionsOverrideUsername :: Maybe Text
    <*> arbitraryReducedMaybe n -- sandboxPublicTokenCreateRequestOptionsOverridePassword :: Maybe Text
    <*> arbitraryReducedMaybe n -- sandboxPublicTokenCreateRequestOptionsTransactions :: Maybe SandboxPublicTokenCreateRequestOptionsTransactions
  
instance Arbitrary SandboxPublicTokenCreateRequestOptionsTransactions where
  arbitrary = sized genSandboxPublicTokenCreateRequestOptionsTransactions

genSandboxPublicTokenCreateRequestOptionsTransactions :: Int -> Gen SandboxPublicTokenCreateRequestOptionsTransactions
genSandboxPublicTokenCreateRequestOptionsTransactions n =
  SandboxPublicTokenCreateRequestOptionsTransactions
    <$> arbitraryReducedMaybe n -- sandboxPublicTokenCreateRequestOptionsTransactionsStartDate :: Maybe Text
    <*> arbitraryReducedMaybe n -- sandboxPublicTokenCreateRequestOptionsTransactionsEndDate :: Maybe Text
  
instance Arbitrary SandboxPublicTokenCreateResponse where
  arbitrary = sized genSandboxPublicTokenCreateResponse

genSandboxPublicTokenCreateResponse :: Int -> Gen SandboxPublicTokenCreateResponse
genSandboxPublicTokenCreateResponse n =
  SandboxPublicTokenCreateResponse
    <$> arbitrary -- sandboxPublicTokenCreateResponsePublicToken :: Text
    <*> arbitrary -- sandboxPublicTokenCreateResponseRequestId :: Text
  
instance Arbitrary Security where
  arbitrary = sized genSecurity

genSecurity :: Int -> Gen Security
genSecurity n =
  Security
    <$> arbitrary -- securitySecurityId :: Text
    <*> arbitraryReducedMaybe n -- securityIsin :: Maybe Text
    <*> arbitraryReducedMaybe n -- securityCusip :: Maybe Text
    <*> arbitraryReducedMaybe n -- securitySedol :: Maybe Text
    <*> arbitraryReducedMaybe n -- securityInstitutionSecurityId :: Maybe Text
    <*> arbitraryReducedMaybe n -- securityInstitutionId :: Maybe Text
    <*> arbitraryReducedMaybe n -- securityProxySecurityId :: Maybe Text
    <*> arbitraryReducedMaybe n -- securityName :: Maybe Text
    <*> arbitraryReducedMaybe n -- securityTickerSymbol :: Maybe Text
    <*> arbitraryReducedMaybe n -- securityIsCashEquivalent :: Maybe Bool
    <*> arbitrary -- securityType :: Text
    <*> arbitraryReducedMaybe n -- securityClosePrice :: Maybe Double
    <*> arbitraryReducedMaybe n -- securityClosePriceAsOf :: Maybe Text
    <*> arbitraryReducedMaybe n -- securityIsoCurrencyCode :: Maybe Text
    <*> arbitraryReducedMaybe n -- securityUnofficialCurrencyCode :: Maybe Text
  
instance Arbitrary ServicerAddressData where
  arbitrary = sized genServicerAddressData

genServicerAddressData :: Int -> Gen ServicerAddressData
genServicerAddressData n =
  ServicerAddressData
    <$> arbitraryReducedMaybe n -- servicerAddressDataCity :: Maybe Text
    <*> arbitraryReducedMaybe n -- servicerAddressDataRegion :: Maybe Text
    <*> arbitraryReducedMaybe n -- servicerAddressDataStreet :: Maybe Text
    <*> arbitraryReducedMaybe n -- servicerAddressDataPostalCode :: Maybe Text
    <*> arbitraryReducedMaybe n -- servicerAddressDataCountry :: Maybe Text
  
instance Arbitrary StandaloneAccountType where
  arbitrary = sized genStandaloneAccountType

genStandaloneAccountType :: Int -> Gen StandaloneAccountType
genStandaloneAccountType n =
  StandaloneAccountType
    <$> arbitrary -- standaloneAccountTypeDepository :: Text
    <*> arbitrary -- standaloneAccountTypeCredit :: Text
    <*> arbitrary -- standaloneAccountTypeLoan :: Text
    <*> arbitrary -- standaloneAccountTypeInvestment :: Text
    <*> arbitrary -- standaloneAccountTypeOther :: Text
  
instance Arbitrary StandaloneCurrencyCodeList where
  arbitrary = sized genStandaloneCurrencyCodeList

genStandaloneCurrencyCodeList :: Int -> Gen StandaloneCurrencyCodeList
genStandaloneCurrencyCodeList n =
  StandaloneCurrencyCodeList
    <$> arbitrary -- standaloneCurrencyCodeListIsoCurrencyCode :: Text
    <*> arbitrary -- standaloneCurrencyCodeListUnofficialCurrencyCode :: Text
  
instance Arbitrary StandaloneInvestmentTransactionSubtype where
  arbitrary = sized genStandaloneInvestmentTransactionSubtype

genStandaloneInvestmentTransactionSubtype :: Int -> Gen StandaloneInvestmentTransactionSubtype
genStandaloneInvestmentTransactionSubtype n =
  StandaloneInvestmentTransactionSubtype
    <$> arbitraryReducedMaybe n -- standaloneInvestmentTransactionSubtypeAccountFee :: Maybe Text
    <*> arbitraryReducedMaybe n -- standaloneInvestmentTransactionSubtypeAssignment :: Maybe Text
    <*> arbitraryReducedMaybe n -- standaloneInvestmentTransactionSubtypeBuy :: Maybe Text
    <*> arbitraryReducedMaybe n -- standaloneInvestmentTransactionSubtypeBuyToCover :: Maybe Text
    <*> arbitraryReducedMaybe n -- standaloneInvestmentTransactionSubtypeContribution :: Maybe Text
    <*> arbitraryReducedMaybe n -- standaloneInvestmentTransactionSubtypeDeposit :: Maybe Text
    <*> arbitraryReducedMaybe n -- standaloneInvestmentTransactionSubtypeDistribution :: Maybe Text
    <*> arbitraryReducedMaybe n -- standaloneInvestmentTransactionSubtypeDividend :: Maybe Text
    <*> arbitraryReducedMaybe n -- standaloneInvestmentTransactionSubtypeDividendReinvestment :: Maybe Text
    <*> arbitraryReducedMaybe n -- standaloneInvestmentTransactionSubtypeExercise :: Maybe Text
    <*> arbitraryReducedMaybe n -- standaloneInvestmentTransactionSubtypeExpire :: Maybe Text
    <*> arbitraryReducedMaybe n -- standaloneInvestmentTransactionSubtypeFundFee :: Maybe Text
    <*> arbitraryReducedMaybe n -- standaloneInvestmentTransactionSubtypeInterest :: Maybe Text
    <*> arbitraryReducedMaybe n -- standaloneInvestmentTransactionSubtypeInterestReceivable :: Maybe Text
    <*> arbitraryReducedMaybe n -- standaloneInvestmentTransactionSubtypeInterestReinvestment :: Maybe Text
    <*> arbitraryReducedMaybe n -- standaloneInvestmentTransactionSubtypeLegalFee :: Maybe Text
    <*> arbitraryReducedMaybe n -- standaloneInvestmentTransactionSubtypeLoanPayment :: Maybe Text
    <*> arbitraryReducedMaybe n -- standaloneInvestmentTransactionSubtypeLongTermCapitalGain :: Maybe Text
    <*> arbitraryReducedMaybe n -- standaloneInvestmentTransactionSubtypeLongTermCapitalGainReinvestment :: Maybe Text
    <*> arbitraryReducedMaybe n -- standaloneInvestmentTransactionSubtypeManagementFee :: Maybe Text
    <*> arbitraryReducedMaybe n -- standaloneInvestmentTransactionSubtypeMarginExpense :: Maybe Text
    <*> arbitraryReducedMaybe n -- standaloneInvestmentTransactionSubtypeMerger :: Maybe Text
    <*> arbitraryReducedMaybe n -- standaloneInvestmentTransactionSubtypeMiscellaneousFee :: Maybe Text
    <*> arbitraryReducedMaybe n -- standaloneInvestmentTransactionSubtypeNonQualifiedDividend :: Maybe Text
    <*> arbitraryReducedMaybe n -- standaloneInvestmentTransactionSubtypeNonResidentTax :: Maybe Text
    <*> arbitraryReducedMaybe n -- standaloneInvestmentTransactionSubtypePendingCredit :: Maybe Text
    <*> arbitraryReducedMaybe n -- standaloneInvestmentTransactionSubtypePendingDebit :: Maybe Text
    <*> arbitraryReducedMaybe n -- standaloneInvestmentTransactionSubtypeQualifiedDividend :: Maybe Text
    <*> arbitraryReducedMaybe n -- standaloneInvestmentTransactionSubtypeRebalance :: Maybe Text
    <*> arbitraryReducedMaybe n -- standaloneInvestmentTransactionSubtypeReturnOfPrincipal :: Maybe Text
    <*> arbitraryReducedMaybe n -- standaloneInvestmentTransactionSubtypeSell :: Maybe Text
    <*> arbitraryReducedMaybe n -- standaloneInvestmentTransactionSubtypeSellShort :: Maybe Text
    <*> arbitraryReducedMaybe n -- standaloneInvestmentTransactionSubtypeShortTermCapitalGain :: Maybe Text
    <*> arbitraryReducedMaybe n -- standaloneInvestmentTransactionSubtypeShortTermCapitalGainReinvestment :: Maybe Text
    <*> arbitraryReducedMaybe n -- standaloneInvestmentTransactionSubtypeSpinOff :: Maybe Text
    <*> arbitraryReducedMaybe n -- standaloneInvestmentTransactionSubtypeSplit :: Maybe Text
    <*> arbitraryReducedMaybe n -- standaloneInvestmentTransactionSubtypeStockDistribution :: Maybe Text
    <*> arbitraryReducedMaybe n -- standaloneInvestmentTransactionSubtypeTax :: Maybe Text
    <*> arbitraryReducedMaybe n -- standaloneInvestmentTransactionSubtypeTaxWithheld :: Maybe Text
    <*> arbitraryReducedMaybe n -- standaloneInvestmentTransactionSubtypeTransfer :: Maybe Text
    <*> arbitraryReducedMaybe n -- standaloneInvestmentTransactionSubtypeTransferFee :: Maybe Text
    <*> arbitraryReducedMaybe n -- standaloneInvestmentTransactionSubtypeTrustFee :: Maybe Text
    <*> arbitraryReducedMaybe n -- standaloneInvestmentTransactionSubtypeUnqualifiedGain :: Maybe Text
    <*> arbitraryReducedMaybe n -- standaloneInvestmentTransactionSubtypeWithdrawal :: Maybe Text
  
instance Arbitrary StandaloneInvestmentTransactionType where
  arbitrary = sized genStandaloneInvestmentTransactionType

genStandaloneInvestmentTransactionType :: Int -> Gen StandaloneInvestmentTransactionType
genStandaloneInvestmentTransactionType n =
  StandaloneInvestmentTransactionType
    <$> arbitrary -- standaloneInvestmentTransactionTypeBuy :: Text
    <*> arbitrary -- standaloneInvestmentTransactionTypeSell :: Text
    <*> arbitrary -- standaloneInvestmentTransactionTypeCancel :: Text
    <*> arbitrary -- standaloneInvestmentTransactionTypeCash :: Text
    <*> arbitrary -- standaloneInvestmentTransactionTypeFee :: Text
    <*> arbitrary -- standaloneInvestmentTransactionTypeTransfer :: Text
  
instance Arbitrary StudentLoan where
  arbitrary = sized genStudentLoan

genStudentLoan :: Int -> Gen StudentLoan
genStudentLoan n =
  StudentLoan
    <$> arbitraryReducedMaybe n -- studentLoanAccountId :: Maybe Text
    <*> arbitraryReducedMaybe n -- studentLoanAccountNumber :: Maybe Text
    <*> arbitraryReducedMaybe n -- studentLoanDisbursementDates :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- studentLoanExpectedPayoffDate :: Maybe Text
    <*> arbitraryReducedMaybe n -- studentLoanGuarantor :: Maybe Text
    <*> arbitrary -- studentLoanInterestRatePercentage :: Double
    <*> arbitraryReducedMaybe n -- studentLoanIsOverdue :: Maybe Bool
    <*> arbitraryReducedMaybe n -- studentLoanLastPaymentAmount :: Maybe Double
    <*> arbitraryReducedMaybe n -- studentLoanLastPaymentDate :: Maybe Text
    <*> arbitraryReducedMaybe n -- studentLoanLastStatementBalance :: Maybe Double
    <*> arbitraryReducedMaybe n -- studentLoanLastStatementIssueDate :: Maybe Text
    <*> arbitraryReducedMaybe n -- studentLoanLoanName :: Maybe Text
    <*> arbitraryReducedMaybe n -- studentLoanLoanStatus :: Maybe StudentLoanStatus
    <*> arbitraryReducedMaybe n -- studentLoanMinimumPaymentAmount :: Maybe Double
    <*> arbitraryReducedMaybe n -- studentLoanNextPaymentDueDate :: Maybe Text
    <*> arbitraryReducedMaybe n -- studentLoanOriginationDate :: Maybe Text
    <*> arbitraryReducedMaybe n -- studentLoanOriginationPrincipalAmount :: Maybe Double
    <*> arbitraryReducedMaybe n -- studentLoanOutstandingInterestAmount :: Maybe Double
    <*> arbitraryReducedMaybe n -- studentLoanPaymentReferenceNumber :: Maybe Text
    <*> arbitraryReducedMaybe n -- studentLoanPslfStatus :: Maybe PSLFStatus
    <*> arbitraryReducedMaybe n -- studentLoanRepaymentPlan :: Maybe StudentRepaymentPlan
    <*> arbitraryReducedMaybe n -- studentLoanSequenceNumber :: Maybe Text
    <*> arbitraryReducedMaybe n -- studentLoanServicerAddress :: Maybe ServicerAddressData
    <*> arbitraryReducedMaybe n -- studentLoanYtdInterestPaid :: Maybe Double
    <*> arbitraryReducedMaybe n -- studentLoanYtdPrincipalPaid :: Maybe Double
  
instance Arbitrary StudentLoanRepaymentModel where
  arbitrary = sized genStudentLoanRepaymentModel

genStudentLoanRepaymentModel :: Int -> Gen StudentLoanRepaymentModel
genStudentLoanRepaymentModel n =
  StudentLoanRepaymentModel
    <$> arbitrary -- studentLoanRepaymentModelType :: Text
    <*> arbitrary -- studentLoanRepaymentModelNonRepaymentMonths :: Double
    <*> arbitrary -- studentLoanRepaymentModelRepaymentMonths :: Double
  
instance Arbitrary StudentLoanStatus where
  arbitrary = sized genStudentLoanStatus

genStudentLoanStatus :: Int -> Gen StudentLoanStatus
genStudentLoanStatus n =
  StudentLoanStatus
    <$> arbitraryReducedMaybe n -- studentLoanStatusEndDate :: Maybe Text
    <*> arbitraryReducedMaybe n -- studentLoanStatusType :: Maybe E'Type3
  
instance Arbitrary StudentRepaymentPlan where
  arbitrary = sized genStudentRepaymentPlan

genStudentRepaymentPlan :: Int -> Gen StudentRepaymentPlan
genStudentRepaymentPlan n =
  StudentRepaymentPlan
    <$> arbitraryReducedMaybe n -- studentRepaymentPlanDescription :: Maybe Text
    <*> arbitraryReducedMaybe n -- studentRepaymentPlanType :: Maybe E'Type4


instance Arbitrary PersonalFinanceCategory where
  arbitrary = genPersonalFinanceCategory

genPersonalFinanceCategory :: Gen PersonalFinanceCategory
genPersonalFinanceCategory =
  PersonalFinanceCategory
    <$> arbitrary -- personalFinanceCategoryPrimary :: Text
    <*> arbitrary -- personalFinanceCategoryDetailed :: Text
  
instance Arbitrary Transaction where
  arbitrary = sized genTransaction

genTransaction :: Int -> Gen Transaction
genTransaction n =
  Transaction
    <$> arbitraryReducedMaybe n -- transactionTransactionType :: Maybe E'TransactionType
    <*> arbitrary -- transactionTransactionId :: Text
    <*> arbitraryReducedMaybe n -- transactionAccountOwner :: Maybe Text
    <*> arbitraryReducedMaybe n -- transactionPendingTransactionId :: Maybe Text
    <*> arbitrary -- transactionPending :: Bool
    <*> arbitraryReducedMaybe n -- transactionPaymentChannel :: Maybe E'PaymentChannel
    <*> arbitraryReducedMaybe n -- transactionPaymentMeta :: Maybe PaymentMeta
    <*> arbitraryReducedMaybe n -- transactionName :: Maybe Text
    <*> arbitraryReducedMaybe n -- transactionMerchantName :: Maybe Text
    <*> arbitraryReducedMaybe n -- transactionLocation :: Maybe Location
    <*> arbitraryReducedMaybe n -- transactionAuthorizedDate :: Maybe Text
    <*> arbitraryReducedMaybe n -- transactionAuthorizedDatetime :: Maybe Text
    <*> arbitrary -- transactionDate :: Text
    <*> arbitraryReducedMaybe n -- transactionDatetime :: Maybe Text
    <*> arbitraryReducedMaybe n -- transactionCategoryId :: Maybe Text
    <*> arbitraryReducedMaybe n -- transactionPersonalFinanceCategory :: Maybe PersonalFinanceCategory
    <*> arbitraryReducedMaybe n -- transactionCategory :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- transactionUnofficialCurrencyCode :: Maybe Text
    <*> arbitraryReducedMaybe n -- transactionIsoCurrencyCode :: Maybe Text
    <*> arbitrary -- transactionAmount :: Double
    <*> arbitrary -- transactionAccountId :: Text
    <*> arbitraryReducedMaybe n -- transactionTransactionCode :: Maybe TransactionCode
  
instance Arbitrary TransactionData where
  arbitrary = sized genTransactionData

genTransactionData :: Int -> Gen TransactionData
genTransactionData n =
  TransactionData
    <$> arbitrary -- transactionDataDescription :: Text
    <*> arbitrary -- transactionDataAmount :: Double
    <*> arbitrary -- transactionDataDate :: Text
    <*> arbitrary -- transactionDataAccountId :: Text
    <*> arbitrary -- transactionDataTransactionId :: Text
  
instance Arbitrary TransactionOverride where
  arbitrary = sized genTransactionOverride

genTransactionOverride :: Int -> Gen TransactionOverride
genTransactionOverride n =
  TransactionOverride
    <$> arbitrary -- transactionOverrideTransactionDate :: Text
    <*> arbitrary -- transactionOverridePostedDate :: Text
    <*> arbitrary -- transactionOverrideAmount :: Double
    <*> arbitrary -- transactionOverrideDescription :: Text
    <*> arbitraryReducedMaybe n -- transactionOverrideCurrency :: Maybe Text
  
instance Arbitrary TransactionsGetRequest where
  arbitrary = sized genTransactionsGetRequest

genTransactionsGetRequest :: Int -> Gen TransactionsGetRequest
genTransactionsGetRequest n =
  TransactionsGetRequest
    <$> arbitraryReducedMaybe n -- transactionsGetRequestClientId :: Maybe Text
    <*> arbitraryReducedMaybe n -- transactionsGetRequestOptions :: Maybe TransactionsGetRequestOptions
    <*> arbitrary -- transactionsGetRequestAccessToken :: Text
    <*> arbitraryReducedMaybe n -- transactionsGetRequestSecret :: Maybe Text
    <*> arbitraryReduced n -- transactionsGetRequestStartDate :: Date
    <*> arbitraryReduced n -- transactionsGetRequestEndDate :: Date
  
instance Arbitrary TransactionsGetRequestOptions where
  arbitrary = sized genTransactionsGetRequestOptions

genTransactionsGetRequestOptions :: Int -> Gen TransactionsGetRequestOptions
genTransactionsGetRequestOptions n =
  TransactionsGetRequestOptions
    <$> arbitraryReducedMaybe n -- transactionsGetRequestOptionsAccountIds :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- transactionsGetRequestOptionsCount :: Maybe Int
    <*> arbitraryReducedMaybe n -- transactionsGetRequestOptionsOffset :: Maybe Int
    <*> arbitrary -- transactionsGetRequestOptionsIncludePersonalFinanceCategory :: Bool
    <*> arbitraryReducedMaybe n -- transactionsGetRequestOptionsDaysRequested :: Maybe Int
  
instance Arbitrary TransactionsGetResponse where
  arbitrary = sized genTransactionsGetResponse

genTransactionsGetResponse :: Int -> Gen TransactionsGetResponse
genTransactionsGetResponse n =
  TransactionsGetResponse
    <$> arbitraryReduced n -- transactionsGetResponseAccounts :: [AccountBase]
    <*> arbitraryReduced n -- transactionsGetResponseTransactions :: [Transaction]
    <*> arbitrary -- transactionsGetResponseTotalTransactions :: Int
    <*> arbitraryReduced n -- transactionsGetResponseItem :: Item
    <*> arbitrary -- transactionsGetResponseRequestId :: Text
  
instance Arbitrary TransactionsRefreshRequest where
  arbitrary = sized genTransactionsRefreshRequest

genTransactionsRefreshRequest :: Int -> Gen TransactionsRefreshRequest
genTransactionsRefreshRequest n =
  TransactionsRefreshRequest
    <$> arbitraryReducedMaybe n -- transactionsRefreshRequestClientId :: Maybe Text
    <*> arbitrary -- transactionsRefreshRequestAccessToken :: Text
    <*> arbitraryReducedMaybe n -- transactionsRefreshRequestSecret :: Maybe Text
  
instance Arbitrary TransactionsRefreshResponse where
  arbitrary = sized genTransactionsRefreshResponse

genTransactionsRefreshResponse :: Int -> Gen TransactionsRefreshResponse
genTransactionsRefreshResponse n =
  TransactionsRefreshResponse
    <$> arbitrary -- transactionsRefreshResponseRequestId :: Text
  
instance Arbitrary TransactionsRemovedWebhook where
  arbitrary = sized genTransactionsRemovedWebhook

genTransactionsRemovedWebhook :: Int -> Gen TransactionsRemovedWebhook
genTransactionsRemovedWebhook n =
  TransactionsRemovedWebhook
    <$> arbitrary -- transactionsRemovedWebhookWebhookType :: Text
    <*> arbitrary -- transactionsRemovedWebhookWebhookCode :: Text
    <*> arbitraryReducedMaybe n -- transactionsRemovedWebhookError :: Maybe Error
    <*> arbitrary -- transactionsRemovedWebhookRemovedTransactions :: [Text]
    <*> arbitrary -- transactionsRemovedWebhookItemId :: Text

instance Arbitrary Cursor where
  arbitrary = Cursor <$> arbitrary
  
instance Arbitrary TransactionsSyncRequest where
  arbitrary = sized genTransactionsSyncRequest

genTransactionsSyncRequest :: Int -> Gen TransactionsSyncRequest
genTransactionsSyncRequest n =
  TransactionsSyncRequest
    <$> arbitraryReducedMaybe n -- transactionsSyncRequestClientId :: Maybe Text
    <*> arbitrary -- transactionsSyncRequestAccessToken :: Text
    <*> arbitraryReducedMaybe n -- transactionsSyncRequestSecret :: Maybe Text
    <*> arbitraryReducedMaybe n -- transactionsSyncRequestCursor :: Maybe Cursor
    <*> arbitraryReducedMaybe n -- transactionsSyncRequestCount :: Maybe Int
    <*> arbitraryReducedMaybe n -- transactionsSyncRequestOptions :: Maybe TransactionsSyncRequestOptions
  
instance Arbitrary TransactionsSyncRequestOptions where
  arbitrary = sized genTransactionsSyncRequestOptions

genTransactionsSyncRequestOptions :: Int -> Gen TransactionsSyncRequestOptions
genTransactionsSyncRequestOptions n =
  TransactionsSyncRequestOptions
    <$> arbitraryReducedMaybe n -- transactionsSyncRequestOptionsIncludeOriginalDescription :: Maybe Bool
    <*> arbitraryReducedMaybe n -- transactionsSyncRequestOptionsIncludePersonalFinanceCategory :: Maybe Bool
    <*> arbitraryReducedMaybe n -- transactionsSyncRequestOptionsDaysRequested :: Maybe Int
  
instance Arbitrary TransactionsSyncResponse where
  arbitrary = sized genTransactionsSyncResponse

genTransactionsSyncResponse :: Int -> Gen TransactionsSyncResponse
genTransactionsSyncResponse n =
  TransactionsSyncResponse
    <$> arbitraryReduced n -- transactionsSyncResponseAdded :: [Transaction]
    <*> arbitraryReduced n -- transactionsSyncResponseModified :: [Transaction]
    <*> arbitraryReduced n -- transactionsSyncResponseRemoved :: [RemovedTransaction]
    <*> arbitrary -- transactionsSyncResponseNextCursor :: Text
    <*> arbitrary -- transactionsSyncResponseHasMore :: Bool
    <*> arbitrary -- transactionsSyncResponseRequestId :: Text
  
instance Arbitrary UserCustomPassword where
  arbitrary = sized genUserCustomPassword

genUserCustomPassword :: Int -> Gen UserCustomPassword
genUserCustomPassword n =
  UserCustomPassword
    <$> arbitraryReducedMaybe n -- userCustomPasswordVersion :: Maybe Text
    <*> arbitrary -- userCustomPasswordSeed :: Text
    <*> arbitraryReduced n -- userCustomPasswordOverrideAccounts :: [OverrideAccounts]
    <*> arbitraryReduced n -- userCustomPasswordMfa :: MFA
    <*> arbitrary -- userCustomPasswordRecaptcha :: Text
    <*> arbitrary -- userCustomPasswordForceError :: Text
  
instance Arbitrary UserPermissionRevokedWebhook where
  arbitrary = sized genUserPermissionRevokedWebhook

genUserPermissionRevokedWebhook :: Int -> Gen UserPermissionRevokedWebhook
genUserPermissionRevokedWebhook n =
  UserPermissionRevokedWebhook
    <$> arbitrary -- userPermissionRevokedWebhookWebhookType :: Text
    <*> arbitrary -- userPermissionRevokedWebhookWebhookCode :: Text
    <*> arbitrary -- userPermissionRevokedWebhookItemId :: Text
    <*> arbitraryReducedMaybe n -- userPermissionRevokedWebhookError :: Maybe Error
  
instance Arbitrary VerificationExpiredWebhook where
  arbitrary = sized genVerificationExpiredWebhook

genVerificationExpiredWebhook :: Int -> Gen VerificationExpiredWebhook
genVerificationExpiredWebhook n =
  VerificationExpiredWebhook
    <$> arbitrary -- verificationExpiredWebhookWebhookType :: Text
    <*> arbitrary -- verificationExpiredWebhookWebhookCode :: Text
    <*> arbitrary -- verificationExpiredWebhookItemId :: Text
    <*> arbitrary -- verificationExpiredWebhookAccountId :: Text
  
instance Arbitrary Warning where
  arbitrary = sized genWarning

genWarning :: Int -> Gen Warning
genWarning n =
  Warning
    <$> arbitrary -- warningWarningType :: Text
    <*> arbitrary -- warningWarningCode :: Text
    <*> arbitraryReduced n -- warningCause :: Cause
  
instance Arbitrary WebhookUpdateAcknowledgedWebhook where
  arbitrary = sized genWebhookUpdateAcknowledgedWebhook

genWebhookUpdateAcknowledgedWebhook :: Int -> Gen WebhookUpdateAcknowledgedWebhook
genWebhookUpdateAcknowledgedWebhook n =
  WebhookUpdateAcknowledgedWebhook
    <$> arbitrary -- webhookUpdateAcknowledgedWebhookWebhookType :: Text
    <*> arbitrary -- webhookUpdateAcknowledgedWebhookWebhookCode :: Text
    <*> arbitrary -- webhookUpdateAcknowledgedWebhookItemId :: Text
    <*> arbitrary -- webhookUpdateAcknowledgedWebhookNewWebhookUrl :: Text
    <*> arbitraryReducedMaybe n -- webhookUpdateAcknowledgedWebhookError :: Maybe Error
  
instance Arbitrary WebhookVerificationKeyGetRequest where
  arbitrary = sized genWebhookVerificationKeyGetRequest

genWebhookVerificationKeyGetRequest :: Int -> Gen WebhookVerificationKeyGetRequest
genWebhookVerificationKeyGetRequest n =
  WebhookVerificationKeyGetRequest
    <$> arbitraryReducedMaybe n -- webhookVerificationKeyGetRequestClientId :: Maybe Text
    <*> arbitraryReducedMaybe n -- webhookVerificationKeyGetRequestSecret :: Maybe Text
    <*> arbitrary -- webhookVerificationKeyGetRequestKeyId :: Text
  
instance Arbitrary WebhookVerificationKeyGetResponse where
  arbitrary = sized genWebhookVerificationKeyGetResponse

genWebhookVerificationKeyGetResponse :: Int -> Gen WebhookVerificationKeyGetResponse
genWebhookVerificationKeyGetResponse n =
  WebhookVerificationKeyGetResponse
    <$> arbitraryReduced n -- webhookVerificationKeyGetResponseKey :: JWKPublicKey
    <*> arbitrary -- webhookVerificationKeyGetResponseRequestId :: Text
  
instance Arbitrary YTDGrossIncomeSummaryFieldNumber where
  arbitrary = sized genYTDGrossIncomeSummaryFieldNumber

genYTDGrossIncomeSummaryFieldNumber :: Int -> Gen YTDGrossIncomeSummaryFieldNumber
genYTDGrossIncomeSummaryFieldNumber n =
  YTDGrossIncomeSummaryFieldNumber
    <$> arbitrary -- yTDGrossIncomeSummaryFieldNumberValue :: Double
    <*> arbitraryReduced n -- yTDGrossIncomeSummaryFieldNumberVerificationStatus :: VerificationStatus
  
instance Arbitrary YTDNetIncomeSummaryFieldNumber where
  arbitrary = sized genYTDNetIncomeSummaryFieldNumber

genYTDNetIncomeSummaryFieldNumber :: Int -> Gen YTDNetIncomeSummaryFieldNumber
genYTDNetIncomeSummaryFieldNumber n =
  YTDNetIncomeSummaryFieldNumber
    <$> arbitrary -- yTDNetIncomeSummaryFieldNumberValue :: Double
    <*> arbitraryReduced n -- yTDNetIncomeSummaryFieldNumberVerificationStatus :: VerificationStatus
  



instance Arbitrary ACHClass where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary AccountSubtype where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary AccountType where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary BankTransferDirection where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary BankTransferEventType where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary BankTransferNetwork where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary BankTransferStatus where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary BankTransferType where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary CountryCode where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'AccountSubtype where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'AprType where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'AvailableBalance where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'BankTransferType where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'Currency where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'ErrorType where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'PaymentChannel where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'RefreshInterval where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'State where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'Status where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'Status2 where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'Status3 where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'Subtype where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'TransactionType where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'Type where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'Type2 where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'Type3 where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'Type4 where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'Type5 where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'UpdateType where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'Value where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'VerificationStatus where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'VerificationStatus2 where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'WebhookCode where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary Products where
  arbitrary = arbitraryBoundedEnum
  
instance Arbitrary RequiredIfSupportedProducts where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary AdditionalConsentedProducts where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary TransactionCode where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary VerificationStatus where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary BankInitiatedReturnRisk where
  arbitrary = sized genBankInitiatedReturnRisk

genBankInitiatedReturnRisk :: Int -> Gen BankInitiatedReturnRisk
genBankInitiatedReturnRisk n =
  BankInitiatedReturnRisk
    <$> arbitrary -- bankInitiatedReturnRiskRiskTier :: Int
    <*> arbitrary -- bankInitiatedReturnRiskScore :: Int
  
instance Arbitrary BaseReport where
  arbitrary = sized genBaseReport

genBaseReport :: Int -> Gen BaseReport
genBaseReport n =
  BaseReport
    <$> arbitraryReducedMaybe n -- baseReportAttributes :: Maybe BaseReportUserAttributes
    <*> arbitraryReducedMaybe n -- baseReportClientReportId :: Maybe Text
    <*> arbitraryReduced n -- baseReportDateGenerated :: DateTime
    <*> arbitrary -- baseReportDaysRequested :: Double
    <*> arbitraryReduced n -- baseReportItems :: [BaseReportItem]
    <*> arbitrary -- baseReportReportId :: Text
  
instance Arbitrary BaseReportAccount where
  arbitrary = sized genBaseReportAccount

genBaseReportAccount :: Int -> Gen BaseReportAccount
genBaseReportAccount n =
  BaseReportAccount
    <$> arbitrary -- baseReportAccountAccountId :: Text
    <*> arbitraryReducedMaybe n -- baseReportAccountAccountInsights :: Maybe BaseReportAccountInsights
    <*> arbitraryReducedMaybe n -- baseReportAccountAttributes :: Maybe BaseReportAttributes
    <*> arbitraryReduced n -- baseReportAccountBalances :: BaseReportAccountBalances
    <*> arbitraryReduced n -- baseReportAccountConsumerDisputes :: [ConsumerDispute]
    <*> arbitrary -- baseReportAccountDaysAvailable :: Double
    <*> arbitraryReducedMaybe n -- baseReportAccountHistoricalBalances :: Maybe [BaseReportHistoricalBalance]
    <*> arbitrary -- baseReportAccountMask :: Text
    <*> arbitraryReduced n -- baseReportAccountMetadata :: BaseReportAccountMetadata
    <*> arbitrary -- baseReportAccountName :: Text
    <*> arbitrary -- baseReportAccountOfficialName :: Text
    <*> arbitraryReduced n -- baseReportAccountOwners :: [Owner]
    <*> arbitraryReduced n -- baseReportAccountOwnershipType :: OwnershipType
    <*> arbitraryReduced n -- baseReportAccountSubtype :: AccountSubtype
    <*> arbitraryReduced n -- baseReportAccountTransactions :: [BaseReportTransaction]
    <*> arbitraryReduced n -- baseReportAccountType :: AccountType
  
instance Arbitrary BaseReportAccountBalances where
  arbitrary = sized genBaseReportAccountBalances

genBaseReportAccountBalances :: Int -> Gen BaseReportAccountBalances
genBaseReportAccountBalances n =
  BaseReportAccountBalances
    <$> arbitrary -- baseReportAccountBalancesAvailable :: Double
    <*> arbitraryReducedMaybe n -- baseReportAccountBalancesAverageBalance :: Maybe Double
    <*> arbitraryReducedMaybe n -- baseReportAccountBalancesAverageMonthlyBalances :: Maybe [BaseReportAverageMonthlyBalances]
    <*> arbitrary -- baseReportAccountBalancesCurrent :: Double
    <*> arbitrary -- baseReportAccountBalancesIsoCurrencyCode :: Text
    <*> arbitraryReducedMaybe n -- baseReportAccountBalancesLastUpdatedDatetime :: Maybe DateTime
    <*> arbitrary -- baseReportAccountBalancesLimit :: Double
    <*> arbitraryReducedMaybe n -- baseReportAccountBalancesMostRecentThirtyDayAverageBalance :: Maybe Double
    <*> arbitrary -- baseReportAccountBalancesUnofficialCurrencyCode :: Text
  
instance Arbitrary BaseReportAccountInsights where
  arbitrary = sized genBaseReportAccountInsights

genBaseReportAccountInsights :: Int -> Gen BaseReportAccountInsights
genBaseReportAccountInsights n =
  BaseReportAccountInsights
    <$> arbitraryReducedMaybe n -- baseReportAccountInsightsAverageDaysBetweenTransactions :: Maybe Double
    <*> arbitraryReducedMaybe n -- baseReportAccountInsightsAverageInflowAmounts :: Maybe [BaseReportAverageFlowInsights]
    <*> arbitraryReducedMaybe n -- baseReportAccountInsightsAverageOutflowAmounts :: Maybe [BaseReportAverageFlowInsights]
    <*> arbitraryReducedMaybe n -- baseReportAccountInsightsDaysAvailable :: Maybe Int
    <*> arbitraryReducedMaybe n -- baseReportAccountInsightsLongestGapsBetweenTransactions :: Maybe [BaseReportLongestGapInsights]
    <*> arbitraryReducedMaybe n -- baseReportAccountInsightsMostRecentTransactionDate :: Maybe Date
    <*> arbitraryReducedMaybe n -- baseReportAccountInsightsNumberOfDaysNoTransactions :: Maybe Int
    <*> arbitraryReducedMaybe n -- baseReportAccountInsightsNumberOfInflows :: Maybe [BaseReportNumberFlowInsights]
    <*> arbitraryReducedMaybe n -- baseReportAccountInsightsNumberOfOutflows :: Maybe [BaseReportNumberFlowInsights]
    <*> arbitraryReducedMaybe n -- baseReportAccountInsightsOldestTransactionDate :: Maybe Date
  
instance Arbitrary BaseReportAccountMetadata where
  arbitrary = sized genBaseReportAccountMetadata

genBaseReportAccountMetadata :: Int -> Gen BaseReportAccountMetadata
genBaseReportAccountMetadata n =
  BaseReportAccountMetadata
    <$> arbitraryReduced n -- baseReportAccountMetadataEndDate :: Date
    <*> arbitraryReduced n -- baseReportAccountMetadataStartDate :: Date
  
instance Arbitrary BaseReportAttributes where
  arbitrary = sized genBaseReportAttributes

genBaseReportAttributes :: Int -> Gen BaseReportAttributes
genBaseReportAttributes n =
  BaseReportAttributes
    <$> arbitraryReducedMaybe n -- baseReportAttributesIsPrimaryAccount :: Maybe Bool
    <*> arbitraryReducedMaybe n -- baseReportAttributesNsfOverdraftTransactionsCount :: Maybe Int
    <*> arbitraryReducedMaybe n -- baseReportAttributesNsfOverdraftTransactionsCount30d :: Maybe Int
    <*> arbitraryReducedMaybe n -- baseReportAttributesNsfOverdraftTransactionsCount60d :: Maybe Int
    <*> arbitraryReducedMaybe n -- baseReportAttributesNsfOverdraftTransactionsCount90d :: Maybe Int
    <*> arbitraryReducedMaybe n -- baseReportAttributesPrimaryAccountScore :: Maybe Double
    <*> arbitraryReducedMaybe n -- baseReportAttributesTotalInflowAmount :: Maybe TotalInflowAmount
    <*> arbitraryReducedMaybe n -- baseReportAttributesTotalInflowAmount30d :: Maybe TotalInflowAmount30d
    <*> arbitraryReducedMaybe n -- baseReportAttributesTotalInflowAmount60d :: Maybe TotalInflowAmount60d
    <*> arbitraryReducedMaybe n -- baseReportAttributesTotalInflowAmount90d :: Maybe TotalInflowAmount90d
    <*> arbitraryReducedMaybe n -- baseReportAttributesTotalOutflowAmount :: Maybe TotalOutflowAmount
    <*> arbitraryReducedMaybe n -- baseReportAttributesTotalOutflowAmount30d :: Maybe TotalOutflowAmount30d
    <*> arbitraryReducedMaybe n -- baseReportAttributesTotalOutflowAmount60d :: Maybe TotalOutflowAmount60d
    <*> arbitraryReducedMaybe n -- baseReportAttributesTotalOutflowAmount90d :: Maybe TotalOutflowAmount90d
  
instance Arbitrary BaseReportAverageFlowInsights where
  arbitrary = sized genBaseReportAverageFlowInsights

genBaseReportAverageFlowInsights :: Int -> Gen BaseReportAverageFlowInsights
genBaseReportAverageFlowInsights n =
  BaseReportAverageFlowInsights
    <$> arbitraryReduced n -- baseReportAverageFlowInsightsEndDate :: Date
    <*> arbitraryReduced n -- baseReportAverageFlowInsightsStartDate :: Date
    <*> arbitraryReduced n -- baseReportAverageFlowInsightsTotalAmount :: CreditAmountWithCurrency
  
instance Arbitrary BaseReportAverageMonthlyBalances where
  arbitrary = sized genBaseReportAverageMonthlyBalances

genBaseReportAverageMonthlyBalances :: Int -> Gen BaseReportAverageMonthlyBalances
genBaseReportAverageMonthlyBalances n =
  BaseReportAverageMonthlyBalances
    <$> arbitraryReduced n -- baseReportAverageMonthlyBalancesAverageBalance :: CreditAmountWithCurrency
    <*> arbitrary -- baseReportAverageMonthlyBalancesEndDate :: Text
    <*> arbitrary -- baseReportAverageMonthlyBalancesStartDate :: Text
  
instance Arbitrary BaseReportHistoricalBalance where
  arbitrary = sized genBaseReportHistoricalBalance

genBaseReportHistoricalBalance :: Int -> Gen BaseReportHistoricalBalance
genBaseReportHistoricalBalance n =
  BaseReportHistoricalBalance
    <$> arbitrary -- baseReportHistoricalBalanceCurrent :: Double
    <*> arbitraryReduced n -- baseReportHistoricalBalanceDate :: Date
    <*> arbitrary -- baseReportHistoricalBalanceIsoCurrencyCode :: Text
    <*> arbitrary -- baseReportHistoricalBalanceUnofficialCurrencyCode :: Text
  
instance Arbitrary BaseReportItem where
  arbitrary = sized genBaseReportItem

genBaseReportItem :: Int -> Gen BaseReportItem
genBaseReportItem n =
  BaseReportItem
    <$> arbitraryReduced n -- baseReportItemAccounts :: [BaseReportAccount]
    <*> arbitraryReduced n -- baseReportItemDateLastUpdated :: DateTime
    <*> arbitrary -- baseReportItemInstitutionId :: Text
    <*> arbitrary -- baseReportItemInstitutionName :: Text
    <*> arbitrary -- baseReportItemItemId :: Text
  
instance Arbitrary BaseReportLongestGapInsights where
  arbitrary = sized genBaseReportLongestGapInsights

genBaseReportLongestGapInsights :: Int -> Gen BaseReportLongestGapInsights
genBaseReportLongestGapInsights n =
  BaseReportLongestGapInsights
    <$> arbitraryReducedMaybe n -- baseReportLongestGapInsightsDays :: Maybe Int
    <*> arbitraryReducedMaybe n -- baseReportLongestGapInsightsEndDate :: Maybe Date
    <*> arbitraryReducedMaybe n -- baseReportLongestGapInsightsStartDate :: Maybe Date
  
instance Arbitrary BaseReportNumberFlowInsights where
  arbitrary = sized genBaseReportNumberFlowInsights

genBaseReportNumberFlowInsights :: Int -> Gen BaseReportNumberFlowInsights
genBaseReportNumberFlowInsights n =
  BaseReportNumberFlowInsights
    <$> arbitrary -- baseReportNumberFlowInsightsCount :: Int
    <*> arbitraryReduced n -- baseReportNumberFlowInsightsEndDate :: Date
    <*> arbitraryReduced n -- baseReportNumberFlowInsightsStartDate :: Date
  
instance Arbitrary BaseReportTransaction where
  arbitrary = sized genBaseReportTransaction

genBaseReportTransaction :: Int -> Gen BaseReportTransaction
genBaseReportTransaction n =
  BaseReportTransaction
    <$> arbitrary -- baseReportTransactionAccountId :: Text
    <*> arbitraryReducedMaybe n -- baseReportTransactionAccountOwner :: Maybe Text
    <*> arbitrary -- baseReportTransactionAmount :: Double
    <*> arbitraryReducedMaybe n -- baseReportTransactionCategory :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- baseReportTransactionCategoryId :: Maybe Text
    <*> arbitraryReducedMaybe n -- baseReportTransactionCheckNumber :: Maybe Text
    <*> arbitraryReducedMaybe n -- baseReportTransactionCreditCategory :: Maybe CreditCategory
    <*> arbitraryReduced n -- baseReportTransactionDate :: Date
    <*> arbitraryReducedMaybe n -- baseReportTransactionDateTransacted :: Maybe Text
    <*> arbitrary -- baseReportTransactionIsoCurrencyCode :: Text
    <*> arbitraryReducedMaybe n -- baseReportTransactionLocation :: Maybe Location
    <*> arbitraryReducedMaybe n -- baseReportTransactionMerchantName :: Maybe Text
    <*> arbitraryReducedMaybe n -- baseReportTransactionName :: Maybe Text
    <*> arbitrary -- baseReportTransactionOriginalDescription :: Text
    <*> arbitrary -- baseReportTransactionPending :: Bool
    <*> arbitraryReducedMaybe n -- baseReportTransactionPersonalFinanceCategory :: Maybe PersonalFinanceCategory
    <*> arbitrary -- baseReportTransactionTransactionId :: Text
    <*> arbitraryReducedMaybe n -- baseReportTransactionTransactionType :: Maybe BaseReportTransactionType
    <*> arbitrary -- baseReportTransactionUnofficialCurrencyCode :: Text
  
instance Arbitrary BaseReportUserAttributes where
  arbitrary = sized genBaseReportUserAttributes

genBaseReportUserAttributes :: Int -> Gen BaseReportUserAttributes
genBaseReportUserAttributes n =
  BaseReportUserAttributes
    <$> arbitraryReducedMaybe n -- baseReportUserAttributesNsfOverdraftTransactionsCount :: Maybe Int
    <*> arbitraryReducedMaybe n -- baseReportUserAttributesNsfOverdraftTransactionsCount30d :: Maybe Int
    <*> arbitraryReducedMaybe n -- baseReportUserAttributesNsfOverdraftTransactionsCount60d :: Maybe Int
    <*> arbitraryReducedMaybe n -- baseReportUserAttributesNsfOverdraftTransactionsCount90d :: Maybe Int
    <*> arbitraryReducedMaybe n -- baseReportUserAttributesTotalInflowAmount :: Maybe TotalReportInflowAmount
    <*> arbitraryReducedMaybe n -- baseReportUserAttributesTotalInflowAmount30d :: Maybe TotalReportInflowAmount30d
    <*> arbitraryReducedMaybe n -- baseReportUserAttributesTotalInflowAmount60d :: Maybe TotalReportInflowAmount60d
    <*> arbitraryReducedMaybe n -- baseReportUserAttributesTotalInflowAmount90d :: Maybe TotalReportInflowAmount90d
    <*> arbitraryReducedMaybe n -- baseReportUserAttributesTotalOutflowAmount :: Maybe TotalReportOutflowAmount
    <*> arbitraryReducedMaybe n -- baseReportUserAttributesTotalOutflowAmount30d :: Maybe TotalReportOutflowAmount30d
    <*> arbitraryReducedMaybe n -- baseReportUserAttributesTotalOutflowAmount60d :: Maybe TotalReportOutflowAmount60d
    <*> arbitraryReducedMaybe n -- baseReportUserAttributesTotalOutflowAmount90d :: Maybe TotalReportOutflowAmount90d
  
instance Arbitrary BaseReportWarning where
  arbitrary = sized genBaseReportWarning

genBaseReportWarning :: Int -> Gen BaseReportWarning
genBaseReportWarning n =
  BaseReportWarning
    <$> arbitraryReduced n -- baseReportWarningCause :: Cause
    <*> arbitraryReduced n -- baseReportWarningWarningCode :: BaseReportWarningCode
    <*> arbitrary -- baseReportWarningWarningType :: Text
  
instance Arbitrary CheckReportWarning where
  arbitrary = sized genCheckReportWarning

genCheckReportWarning :: Int -> Gen CheckReportWarning
genCheckReportWarning n =
  CheckReportWarning
    <$> arbitraryReduced n -- checkReportWarningCause :: Cause
    <*> arbitraryReduced n -- checkReportWarningWarningCode :: CheckReportWarningCode
    <*> arbitrary -- checkReportWarningWarningType :: Text
  
instance Arbitrary ClientUserIdentity where
  arbitrary = sized genClientUserIdentity

genClientUserIdentity :: Int -> Gen ClientUserIdentity
genClientUserIdentity n =
  ClientUserIdentity
    <$> arbitraryReducedMaybe n -- clientUserIdentityAddresses :: Maybe [ClientUserIdentityAddress]
    <*> arbitraryReducedMaybe n -- clientUserIdentityDateOfBirth :: Maybe Date
    <*> arbitraryReducedMaybe n -- clientUserIdentityEmails :: Maybe [ClientUserIdentityEmail]
    <*> arbitraryReducedMaybe n -- clientUserIdentityIdNumbers :: Maybe [UserIDNumber]
    <*> arbitraryReducedMaybe n -- clientUserIdentityName :: Maybe ClientUserIdentityName
    <*> arbitraryReducedMaybe n -- clientUserIdentityPhoneNumbers :: Maybe [ClientUserIdentityPhoneNumber]
  
instance Arbitrary ClientUserIdentityAddress where
  arbitrary = sized genClientUserIdentityAddress

genClientUserIdentityAddress :: Int -> Gen ClientUserIdentityAddress
genClientUserIdentityAddress n =
  ClientUserIdentityAddress
    <$> arbitraryReducedMaybe n -- clientUserIdentityAddressCity :: Maybe Text
    <*> arbitrary -- clientUserIdentityAddressCountry :: Text
    <*> arbitraryReducedMaybe n -- clientUserIdentityAddressPostalCode :: Maybe Text
    <*> arbitrary -- clientUserIdentityAddressPrimary :: Bool
    <*> arbitraryReducedMaybe n -- clientUserIdentityAddressRegion :: Maybe Text
    <*> arbitraryReducedMaybe n -- clientUserIdentityAddressStreet1 :: Maybe Text
    <*> arbitraryReducedMaybe n -- clientUserIdentityAddressStreet2 :: Maybe Text
  
instance Arbitrary ClientUserIdentityEmail where
  arbitrary = sized genClientUserIdentityEmail

genClientUserIdentityEmail :: Int -> Gen ClientUserIdentityEmail
genClientUserIdentityEmail n =
  ClientUserIdentityEmail
    <$> arbitrary -- clientUserIdentityEmailData :: Text
    <*> arbitrary -- clientUserIdentityEmailPrimary :: Bool
  
instance Arbitrary ClientUserIdentityName where
  arbitrary = sized genClientUserIdentityName

genClientUserIdentityName :: Int -> Gen ClientUserIdentityName
genClientUserIdentityName n =
  ClientUserIdentityName
    <$> arbitrary -- clientUserIdentityNameFamilyName :: Text
    <*> arbitrary -- clientUserIdentityNameGivenName :: Text
  
instance Arbitrary ClientUserIdentityPhoneNumber where
  arbitrary = sized genClientUserIdentityPhoneNumber

genClientUserIdentityPhoneNumber :: Int -> Gen ClientUserIdentityPhoneNumber
genClientUserIdentityPhoneNumber n =
  ClientUserIdentityPhoneNumber
    <$> arbitrary -- clientUserIdentityPhoneNumberData :: Text
    <*> arbitrary -- clientUserIdentityPhoneNumberPrimary :: Bool
  
instance Arbitrary ConsumerDispute where
  arbitrary = sized genConsumerDispute

genConsumerDispute :: Int -> Gen ConsumerDispute
genConsumerDispute n =
  ConsumerDispute
    <$> arbitraryReduced n -- consumerDisputeCategory :: ConsumerDisputeCategory
    <*> arbitrary -- consumerDisputeConsumerDisputeId :: Text
    <*> arbitraryReduced n -- consumerDisputeDisputeFieldCreateDate :: Date
    <*> arbitrary -- consumerDisputeStatement :: Text
  
instance Arbitrary ConsumerReportUserIdentity where
  arbitrary = sized genConsumerReportUserIdentity

genConsumerReportUserIdentity :: Int -> Gen ConsumerReportUserIdentity
genConsumerReportUserIdentity n =
  ConsumerReportUserIdentity
    <$> arbitraryReduced n -- consumerReportUserIdentityDateOfBirth :: Date
    <*> arbitrary -- consumerReportUserIdentityEmails :: [Text]
    <*> arbitrary -- consumerReportUserIdentityFirstName :: Text
    <*> arbitrary -- consumerReportUserIdentityLastName :: Text
    <*> arbitrary -- consumerReportUserIdentityPhoneNumbers :: [Text]
    <*> arbitraryReduced n -- consumerReportUserIdentityPrimaryAddress :: AddressData
    <*> arbitraryReducedMaybe n -- consumerReportUserIdentitySsnFull :: Maybe Text
    <*> arbitraryReducedMaybe n -- consumerReportUserIdentitySsnLast4 :: Maybe Text
  
instance Arbitrary CraAnnualIncomeValues where
  arbitrary = sized genCraAnnualIncomeValues

genCraAnnualIncomeValues :: Int -> Gen CraAnnualIncomeValues
genCraAnnualIncomeValues n =
  CraAnnualIncomeValues
    <$> arbitrary -- craAnnualIncomeValuesGrossIncome :: Double
    <*> arbitrary -- craAnnualIncomeValuesNetIncome :: Double
  
instance Arbitrary CraBankIncomeAccount where
  arbitrary = sized genCraBankIncomeAccount

genCraBankIncomeAccount :: Int -> Gen CraBankIncomeAccount
genCraBankIncomeAccount n =
  CraBankIncomeAccount
    <$> arbitraryReducedMaybe n -- craBankIncomeAccountAccountId :: Maybe Text
    <*> arbitrary -- craBankIncomeAccountMask :: Text
    <*> arbitraryReduced n -- craBankIncomeAccountMetadata :: CraBankIncomeAccountMetadata
    <*> arbitrary -- craBankIncomeAccountName :: Text
    <*> arbitrary -- craBankIncomeAccountOfficialName :: Text
    <*> arbitraryReduced n -- craBankIncomeAccountOwners :: [Owner]
    <*> arbitraryReduced n -- craBankIncomeAccountSubtype :: DepositoryAccountSubtype
    <*> arbitraryReduced n -- craBankIncomeAccountType :: CreditBankIncomeAccountType
  
instance Arbitrary CraBankIncomeAccountMetadata where
  arbitrary = sized genCraBankIncomeAccountMetadata

genCraBankIncomeAccountMetadata :: Int -> Gen CraBankIncomeAccountMetadata
genCraBankIncomeAccountMetadata n =
  CraBankIncomeAccountMetadata
    <$> arbitraryReduced n -- craBankIncomeAccountMetadataEndDate :: Date
    <*> arbitraryReduced n -- craBankIncomeAccountMetadataStartDate :: Date
  
instance Arbitrary CraBankIncomeCause where
  arbitrary = sized genCraBankIncomeCause

genCraBankIncomeCause :: Int -> Gen CraBankIncomeCause
genCraBankIncomeCause n =
  CraBankIncomeCause
    <$> arbitrary -- craBankIncomeCauseDisplayMessage :: Text
    <*> arbitrary -- craBankIncomeCauseErrorCode :: Text
    <*> arbitrary -- craBankIncomeCauseErrorMessage :: Text
    <*> arbitraryReduced n -- craBankIncomeCauseErrorType :: CreditBankIncomeErrorType
  
instance Arbitrary CraBankIncomeEmployer where
  arbitrary = sized genCraBankIncomeEmployer

genCraBankIncomeEmployer :: Int -> Gen CraBankIncomeEmployer
genCraBankIncomeEmployer n =
  CraBankIncomeEmployer
    <$> arbitrary -- craBankIncomeEmployerName :: Text
  
instance Arbitrary CraBankIncomeHistoricalSummary where
  arbitrary = sized genCraBankIncomeHistoricalSummary

genCraBankIncomeHistoricalSummary :: Int -> Gen CraBankIncomeHistoricalSummary
genCraBankIncomeHistoricalSummary n =
  CraBankIncomeHistoricalSummary
    <$> arbitraryReducedMaybe n -- craBankIncomeHistoricalSummaryEndDate :: Maybe Date
    <*> arbitraryReducedMaybe n -- craBankIncomeHistoricalSummaryStartDate :: Maybe Date
    <*> arbitraryReducedMaybe n -- craBankIncomeHistoricalSummaryTotalAmounts :: Maybe [CreditAmountWithCurrency]
    <*> arbitraryReducedMaybe n -- craBankIncomeHistoricalSummaryTransactions :: Maybe [CraBankIncomeTransaction]
  
instance Arbitrary CraBankIncomeIncomeProvider where
  arbitrary = sized genCraBankIncomeIncomeProvider

genCraBankIncomeIncomeProvider :: Int -> Gen CraBankIncomeIncomeProvider
genCraBankIncomeIncomeProvider n =
  CraBankIncomeIncomeProvider
    <$> arbitrary -- craBankIncomeIncomeProviderIsNormalized :: Bool
    <*> arbitrary -- craBankIncomeIncomeProviderName :: Text
  
instance Arbitrary CraBankIncomeItem where
  arbitrary = sized genCraBankIncomeItem

genCraBankIncomeItem :: Int -> Gen CraBankIncomeItem
genCraBankIncomeItem n =
  CraBankIncomeItem
    <$> arbitraryReducedMaybe n -- craBankIncomeItemAccounts :: Maybe [CraBankIncomeAccount]
    <*> arbitraryReduced n -- craBankIncomeItemBankIncomeAccounts :: [CraBankIncomeAccount]
    <*> arbitraryReduced n -- craBankIncomeItemBankIncomeSources :: [CraBankIncomeSource]
    <*> arbitraryReducedMaybe n -- craBankIncomeItemInstitutionId :: Maybe Text
    <*> arbitraryReducedMaybe n -- craBankIncomeItemInstitutionName :: Maybe Text
    <*> arbitraryReducedMaybe n -- craBankIncomeItemItemId :: Maybe Text
    <*> arbitraryReducedMaybe n -- craBankIncomeItemLastUpdatedTime :: Maybe DateTime
  
instance Arbitrary CraBankIncomeSource where
  arbitrary = sized genCraBankIncomeSource

genCraBankIncomeSource :: Int -> Gen CraBankIncomeSource
genCraBankIncomeSource n =
  CraBankIncomeSource
    <$> arbitraryReducedMaybe n -- craBankIncomeSourceAccountId :: Maybe Text
    <*> arbitraryReducedMaybe n -- craBankIncomeSourceEmployer :: Maybe CraBankIncomeEmployer
    <*> arbitraryReducedMaybe n -- craBankIncomeSourceEndDate :: Maybe Date
    <*> arbitraryReducedMaybe n -- craBankIncomeSourceForecastedAverageMonthlyIncome :: Maybe Double
    <*> arbitraryReduced n -- craBankIncomeSourceForecastedAverageMonthlyIncomePredictionIntervals :: [CraPredictionInterval]
    <*> arbitraryReducedMaybe n -- craBankIncomeSourceHistoricalAverageMonthlyGrossIncome :: Maybe Double
    <*> arbitraryReducedMaybe n -- craBankIncomeSourceHistoricalAverageMonthlyIncome :: Maybe Double
    <*> arbitraryReducedMaybe n -- craBankIncomeSourceHistoricalSummary :: Maybe [CraBankIncomeHistoricalSummary]
    <*> arbitraryReducedMaybe n -- craBankIncomeSourceIncomeCategory :: Maybe CreditBankIncomeCategory
    <*> arbitraryReducedMaybe n -- craBankIncomeSourceIncomeDescription :: Maybe Text
    <*> arbitraryReduced n -- craBankIncomeSourceIncomeProvider :: CraBankIncomeIncomeProvider
    <*> arbitraryReducedMaybe n -- craBankIncomeSourceIncomeSourceId :: Maybe Text
    <*> arbitraryReducedMaybe n -- craBankIncomeSourceIsoCurrencyCode :: Maybe Text
    <*> arbitraryReducedMaybe n -- craBankIncomeSourceNextPaymentDate :: Maybe Date
    <*> arbitraryReducedMaybe n -- craBankIncomeSourcePayFrequency :: Maybe CreditBankIncomePayFrequency
    <*> arbitraryReducedMaybe n -- craBankIncomeSourceStartDate :: Maybe Date
    <*> arbitraryReducedMaybe n -- craBankIncomeSourceStatus :: Maybe CraBankIncomeStatus
    <*> arbitraryReducedMaybe n -- craBankIncomeSourceTotalAmount :: Maybe Double
    <*> arbitraryReducedMaybe n -- craBankIncomeSourceTransactionCount :: Maybe Int
    <*> arbitraryReducedMaybe n -- craBankIncomeSourceUnofficialCurrencyCode :: Maybe Text
  
instance Arbitrary CraBankIncomeSummary where
  arbitrary = sized genCraBankIncomeSummary

genCraBankIncomeSummary :: Int -> Gen CraBankIncomeSummary
genCraBankIncomeSummary n =
  CraBankIncomeSummary
    <$> arbitraryReducedMaybe n -- craBankIncomeSummaryEndDate :: Maybe Date
    <*> arbitraryReducedMaybe n -- craBankIncomeSummaryForecastedAnnualIncome :: Maybe [CreditAmountWithCurrency]
    <*> arbitraryReducedMaybe n -- craBankIncomeSummaryForecastedAverageMonthlyIncome :: Maybe [CreditAmountWithCurrency]
    <*> arbitraryReducedMaybe n -- craBankIncomeSummaryHistoricalAnnualGrossIncome :: Maybe [CreditAmountWithCurrency]
    <*> arbitraryReducedMaybe n -- craBankIncomeSummaryHistoricalAnnualIncome :: Maybe [CreditAmountWithCurrency]
    <*> arbitraryReducedMaybe n -- craBankIncomeSummaryHistoricalAverageMonthlyGrossIncome :: Maybe [CreditAmountWithCurrency]
    <*> arbitraryReducedMaybe n -- craBankIncomeSummaryHistoricalAverageMonthlyIncome :: Maybe [CreditAmountWithCurrency]
    <*> arbitraryReducedMaybe n -- craBankIncomeSummaryHistoricalSummary :: Maybe [CraBankIncomeHistoricalSummary]
    <*> arbitraryReducedMaybe n -- craBankIncomeSummaryIncomeCategoriesCount :: Maybe Int
    <*> arbitraryReducedMaybe n -- craBankIncomeSummaryIncomeSourcesCount :: Maybe Int
    <*> arbitraryReducedMaybe n -- craBankIncomeSummaryIncomeTransactionsCount :: Maybe Int
    <*> arbitraryReducedMaybe n -- craBankIncomeSummaryStartDate :: Maybe Date
    <*> arbitraryReducedMaybe n -- craBankIncomeSummaryTotalAmounts :: Maybe [CreditAmountWithCurrency]
  
instance Arbitrary CraBankIncomeTransaction where
  arbitrary = sized genCraBankIncomeTransaction

genCraBankIncomeTransaction :: Int -> Gen CraBankIncomeTransaction
genCraBankIncomeTransaction n =
  CraBankIncomeTransaction
    <$> arbitrary -- craBankIncomeTransactionAmount :: Double
    <*> arbitraryReducedMaybe n -- craBankIncomeTransactionBonusType :: Maybe CraBankIncomeBonusType
    <*> arbitraryReducedMaybe n -- craBankIncomeTransactionCheckNumber :: Maybe Text
    <*> arbitraryReduced n -- craBankIncomeTransactionDate :: Date
    <*> arbitrary -- craBankIncomeTransactionIsoCurrencyCode :: Text
    <*> arbitraryReducedMaybe n -- craBankIncomeTransactionName :: Maybe Text
    <*> arbitrary -- craBankIncomeTransactionOriginalDescription :: Text
    <*> arbitrary -- craBankIncomeTransactionPending :: Bool
    <*> arbitrary -- craBankIncomeTransactionTransactionId :: Text
    <*> arbitrary -- craBankIncomeTransactionUnofficialCurrencyCode :: Text
  
instance Arbitrary CraBankIncomeWarning where
  arbitrary = sized genCraBankIncomeWarning

genCraBankIncomeWarning :: Int -> Gen CraBankIncomeWarning
genCraBankIncomeWarning n =
  CraBankIncomeWarning
    <$> arbitraryReducedMaybe n -- craBankIncomeWarningCause :: Maybe CraBankIncomeCause
    <*> arbitraryReducedMaybe n -- craBankIncomeWarningWarningCode :: Maybe CraBankIncomeWarningCode
    <*> arbitraryReducedMaybe n -- craBankIncomeWarningWarningType :: Maybe CreditBankIncomeWarningType
  
instance Arbitrary CraCheckReportBaseReportGetRequest where
  arbitrary = sized genCraCheckReportBaseReportGetRequest

genCraCheckReportBaseReportGetRequest :: Int -> Gen CraCheckReportBaseReportGetRequest
genCraCheckReportBaseReportGetRequest n =
  CraCheckReportBaseReportGetRequest
    <$> arbitraryReducedMaybe n -- craCheckReportBaseReportGetRequestClientId :: Maybe Text
    <*> arbitraryReducedMaybe n -- craCheckReportBaseReportGetRequestConsumerReportPermissiblePurpose :: Maybe CraCheckReportPermissiblePurpose
    <*> arbitraryReducedMaybe n -- craCheckReportBaseReportGetRequestItemIds :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- craCheckReportBaseReportGetRequestReportId :: Maybe Text
    <*> arbitraryReducedMaybe n -- craCheckReportBaseReportGetRequestSecret :: Maybe Text
    <*> arbitraryReducedMaybe n -- craCheckReportBaseReportGetRequestThirdPartyUserToken :: Maybe Text
    <*> arbitraryReducedMaybe n -- craCheckReportBaseReportGetRequestUserId :: Maybe Text
    <*> arbitraryReducedMaybe n -- craCheckReportBaseReportGetRequestUserTier :: Maybe CraUserTier
    <*> arbitraryReducedMaybe n -- craCheckReportBaseReportGetRequestUserToken :: Maybe Text
  
instance Arbitrary CraCheckReportBaseReportGetResponse where
  arbitrary = sized genCraCheckReportBaseReportGetResponse

genCraCheckReportBaseReportGetResponse :: Int -> Gen CraCheckReportBaseReportGetResponse
genCraCheckReportBaseReportGetResponse n =
  CraCheckReportBaseReportGetResponse
    <$> arbitraryReduced n -- craCheckReportBaseReportGetResponseReport :: BaseReport
    <*> arbitrary -- craCheckReportBaseReportGetResponseRequestId :: Text
    <*> arbitraryReduced n -- craCheckReportBaseReportGetResponseWarnings :: [BaseReportWarning]
  
instance Arbitrary CraCheckReportCreateBaseReportOptions where
  arbitrary = sized genCraCheckReportCreateBaseReportOptions

genCraCheckReportCreateBaseReportOptions :: Int -> Gen CraCheckReportCreateBaseReportOptions
genCraCheckReportCreateBaseReportOptions n =
  CraCheckReportCreateBaseReportOptions
    <$> arbitraryReducedMaybe n -- craCheckReportCreateBaseReportOptionsClientReportId :: Maybe Text
    <*> arbitraryReducedMaybe n -- craCheckReportCreateBaseReportOptionsGseOptions :: Maybe CraCheckReportGSEOptions
    <*> arbitraryReducedMaybe n -- craCheckReportCreateBaseReportOptionsHomeLendingReportOptions :: Maybe CraCheckReportHomeLendingReportOptions
    <*> arbitraryReducedMaybe n -- craCheckReportCreateBaseReportOptionsRequireIdentity :: Maybe Bool
  
instance Arbitrary CraCheckReportCreateCashflowInsightsOptions where
  arbitrary = sized genCraCheckReportCreateCashflowInsightsOptions

genCraCheckReportCreateCashflowInsightsOptions :: Int -> Gen CraCheckReportCreateCashflowInsightsOptions
genCraCheckReportCreateCashflowInsightsOptions n =
  CraCheckReportCreateCashflowInsightsOptions
    <$> arbitraryReducedMaybe n -- craCheckReportCreateCashflowInsightsOptionsAttributesVersion :: Maybe CashflowAttributesVersion
  
instance Arbitrary CraCheckReportCreateEmploymentRefreshOptions where
  arbitrary = sized genCraCheckReportCreateEmploymentRefreshOptions

genCraCheckReportCreateEmploymentRefreshOptions :: Int -> Gen CraCheckReportCreateEmploymentRefreshOptions
genCraCheckReportCreateEmploymentRefreshOptions n =
  CraCheckReportCreateEmploymentRefreshOptions
    <$> arbitrary -- craCheckReportCreateEmploymentRefreshOptionsDaysRequested :: Int
  
instance Arbitrary CraCheckReportCreateIncomeInsightsOptions where
  arbitrary = sized genCraCheckReportCreateIncomeInsightsOptions

genCraCheckReportCreateIncomeInsightsOptions :: Int -> Gen CraCheckReportCreateIncomeInsightsOptions
genCraCheckReportCreateIncomeInsightsOptions n =
  CraCheckReportCreateIncomeInsightsOptions
    <$> arbitraryReducedMaybe n -- craCheckReportCreateIncomeInsightsOptionsIncomeInsightsFilter :: Maybe IncomeInsightsFilter
    <*> arbitraryReduced n -- craCheckReportCreateIncomeInsightsOptionsIncomeInsightsVersion :: IncomeInsightsVersion
  
instance Arbitrary CraCheckReportCreateLendScoreOptions where
  arbitrary = sized genCraCheckReportCreateLendScoreOptions

genCraCheckReportCreateLendScoreOptions :: Int -> Gen CraCheckReportCreateLendScoreOptions
genCraCheckReportCreateLendScoreOptions n =
  CraCheckReportCreateLendScoreOptions
    <$> arbitraryReducedMaybe n -- craCheckReportCreateLendScoreOptionsLendScoreVersion :: Maybe PlaidLendScoreVersion
  
instance Arbitrary CraCheckReportCreateNetworkInsightsOptions where
  arbitrary = sized genCraCheckReportCreateNetworkInsightsOptions

genCraCheckReportCreateNetworkInsightsOptions :: Int -> Gen CraCheckReportCreateNetworkInsightsOptions
genCraCheckReportCreateNetworkInsightsOptions n =
  CraCheckReportCreateNetworkInsightsOptions
    <$> arbitraryReducedMaybe n -- craCheckReportCreateNetworkInsightsOptionsNetworkInsightsVersion :: Maybe NetworkInsightsVersion
  
instance Arbitrary CraCheckReportCreatePartnerInsightsOptions where
  arbitrary = sized genCraCheckReportCreatePartnerInsightsOptions

genCraCheckReportCreatePartnerInsightsOptions :: Int -> Gen CraCheckReportCreatePartnerInsightsOptions
genCraCheckReportCreatePartnerInsightsOptions n =
  CraCheckReportCreatePartnerInsightsOptions
    <$> arbitraryReducedMaybe n -- craCheckReportCreatePartnerInsightsOptionsFico :: Maybe CraPartnerInsightsFicoInput
    <*> arbitraryReducedMaybe n -- craCheckReportCreatePartnerInsightsOptionsPrismVersions :: Maybe PrismVersions
  
instance Arbitrary CraCheckReportCreateRequest where
  arbitrary = sized genCraCheckReportCreateRequest

genCraCheckReportCreateRequest :: Int -> Gen CraCheckReportCreateRequest
genCraCheckReportCreateRequest n =
  CraCheckReportCreateRequest
    <$> arbitraryReducedMaybe n -- craCheckReportCreateRequestBaseReport :: Maybe CraCheckReportCreateBaseReportOptions
    <*> arbitraryReducedMaybe n -- craCheckReportCreateRequestCashflowInsights :: Maybe CraCheckReportCreateCashflowInsightsOptions
    <*> arbitraryReducedMaybe n -- craCheckReportCreateRequestClientId :: Maybe Text
    <*> arbitraryReducedMaybe n -- craCheckReportCreateRequestClientReportId :: Maybe Text
    <*> arbitraryReduced n -- craCheckReportCreateRequestConsumerReportPermissiblePurpose :: ConsumerReportPermissiblePurpose
    <*> arbitrary -- craCheckReportCreateRequestDaysRequested :: Int
    <*> arbitraryReducedMaybe n -- craCheckReportCreateRequestDaysRequired :: Maybe Int
    <*> arbitraryReducedMaybe n -- craCheckReportCreateRequestIncludeInvestments :: Maybe Bool
    <*> arbitraryReducedMaybe n -- craCheckReportCreateRequestIncomeInsights :: Maybe CraCheckReportCreateIncomeInsightsOptions
    <*> arbitraryReducedMaybe n -- craCheckReportCreateRequestLendScore :: Maybe CraCheckReportCreateLendScoreOptions
    <*> arbitraryReducedMaybe n -- craCheckReportCreateRequestNetworkInsights :: Maybe CraCheckReportCreateNetworkInsightsOptions
    <*> arbitraryReducedMaybe n -- craCheckReportCreateRequestPartnerInsights :: Maybe CraCheckReportCreatePartnerInsightsOptions
    <*> arbitraryReducedMaybe n -- craCheckReportCreateRequestProducts :: Maybe [Products]
    <*> arbitraryReducedMaybe n -- craCheckReportCreateRequestSecret :: Maybe Text
    <*> arbitraryReducedMaybe n -- craCheckReportCreateRequestUserId :: Maybe Text
    <*> arbitraryReducedMaybe n -- craCheckReportCreateRequestUserToken :: Maybe Text
    <*> arbitrary -- craCheckReportCreateRequestWebhook :: Text
  
instance Arbitrary CraCheckReportCreateResponse where
  arbitrary = sized genCraCheckReportCreateResponse

genCraCheckReportCreateResponse :: Int -> Gen CraCheckReportCreateResponse
genCraCheckReportCreateResponse n =
  CraCheckReportCreateResponse
    <$> arbitraryReducedMaybe n -- craCheckReportCreateResponseRequestId :: Maybe Text
  
instance Arbitrary CraCheckReportGSEOptions where
  arbitrary = sized genCraCheckReportGSEOptions

genCraCheckReportGSEOptions :: Int -> Gen CraCheckReportGSEOptions
genCraCheckReportGSEOptions n =
  CraCheckReportGSEOptions
    <$> arbitraryReduced n -- craCheckReportGSEOptionsReportTypes :: [GSEReportType]
  
instance Arbitrary CraCheckReportHomeLendingReportOptions where
  arbitrary = sized genCraCheckReportHomeLendingReportOptions

genCraCheckReportHomeLendingReportOptions :: Int -> Gen CraCheckReportHomeLendingReportOptions
genCraCheckReportHomeLendingReportOptions n =
  CraCheckReportHomeLendingReportOptions
    <$> arbitraryReducedMaybe n -- craCheckReportHomeLendingReportOptionsEmploymentRefreshOptions :: Maybe CraCheckReportCreateEmploymentRefreshOptions
    <*> arbitraryReduced n -- craCheckReportHomeLendingReportOptionsReportsRequested :: [CraCheckReportVerificationGetReportType]
  
instance Arbitrary CraCheckReportIncomeInsightsGetOptions where
  arbitrary = sized genCraCheckReportIncomeInsightsGetOptions

genCraCheckReportIncomeInsightsGetOptions :: Int -> Gen CraCheckReportIncomeInsightsGetOptions
genCraCheckReportIncomeInsightsGetOptions n =
  CraCheckReportIncomeInsightsGetOptions
    <$> arbitraryReducedMaybe n -- craCheckReportIncomeInsightsGetOptionsIncomeInsightsFilter :: Maybe IncomeInsightsFilter
    <*> arbitraryReduced n -- craCheckReportIncomeInsightsGetOptionsIncomeInsightsVersion :: IncomeInsightsVersion
  
instance Arbitrary CraCheckReportIncomeInsightsGetRequest where
  arbitrary = sized genCraCheckReportIncomeInsightsGetRequest

genCraCheckReportIncomeInsightsGetRequest :: Int -> Gen CraCheckReportIncomeInsightsGetRequest
genCraCheckReportIncomeInsightsGetRequest n =
  CraCheckReportIncomeInsightsGetRequest
    <$> arbitraryReducedMaybe n -- craCheckReportIncomeInsightsGetRequestClientId :: Maybe Text
    <*> arbitraryReducedMaybe n -- craCheckReportIncomeInsightsGetRequestConsumerReportPermissiblePurpose :: Maybe CraCheckReportPermissiblePurpose
    <*> arbitraryReducedMaybe n -- craCheckReportIncomeInsightsGetRequestOptions :: Maybe CraCheckReportIncomeInsightsGetOptions
    <*> arbitraryReducedMaybe n -- craCheckReportIncomeInsightsGetRequestReportId :: Maybe Text
    <*> arbitraryReducedMaybe n -- craCheckReportIncomeInsightsGetRequestSecret :: Maybe Text
    <*> arbitraryReducedMaybe n -- craCheckReportIncomeInsightsGetRequestThirdPartyUserToken :: Maybe Text
    <*> arbitraryReducedMaybe n -- craCheckReportIncomeInsightsGetRequestUserId :: Maybe Text
    <*> arbitraryReducedMaybe n -- craCheckReportIncomeInsightsGetRequestUserToken :: Maybe Text
  
instance Arbitrary CraCheckReportIncomeInsightsGetResponse where
  arbitrary = sized genCraCheckReportIncomeInsightsGetResponse

genCraCheckReportIncomeInsightsGetResponse :: Int -> Gen CraCheckReportIncomeInsightsGetResponse
genCraCheckReportIncomeInsightsGetResponse n =
  CraCheckReportIncomeInsightsGetResponse
    <$> arbitraryReducedMaybe n -- craCheckReportIncomeInsightsGetResponseReport :: Maybe CraIncomeInsights
    <*> arbitrary -- craCheckReportIncomeInsightsGetResponseRequestId :: Text
    <*> arbitraryReducedMaybe n -- craCheckReportIncomeInsightsGetResponseWarnings :: Maybe [CheckReportWarning]
  
instance Arbitrary CraCheckReportPDFGetRequest where
  arbitrary = sized genCraCheckReportPDFGetRequest

genCraCheckReportPDFGetRequest :: Int -> Gen CraCheckReportPDFGetRequest
genCraCheckReportPDFGetRequest n =
  CraCheckReportPDFGetRequest
    <$> arbitraryReducedMaybe n -- craCheckReportPDFGetRequestAddOns :: Maybe [CraPDFAddOns]
    <*> arbitraryReducedMaybe n -- craCheckReportPDFGetRequestClientId :: Maybe Text
    <*> arbitraryReducedMaybe n -- craCheckReportPDFGetRequestSecret :: Maybe Text
    <*> arbitraryReducedMaybe n -- craCheckReportPDFGetRequestThirdPartyUserToken :: Maybe Text
    <*> arbitraryReducedMaybe n -- craCheckReportPDFGetRequestUserId :: Maybe Text
    <*> arbitraryReducedMaybe n -- craCheckReportPDFGetRequestUserToken :: Maybe Text
  
instance Arbitrary CraCheckReportPartnerInsightsGetOptions where
  arbitrary = sized genCraCheckReportPartnerInsightsGetOptions

genCraCheckReportPartnerInsightsGetOptions :: Int -> Gen CraCheckReportPartnerInsightsGetOptions
genCraCheckReportPartnerInsightsGetOptions n =
  CraCheckReportPartnerInsightsGetOptions
    <$> arbitraryReducedMaybe n -- craCheckReportPartnerInsightsGetOptionsPrismVersions :: Maybe PrismVersionsDeprecated
  
instance Arbitrary CraCheckReportPartnerInsightsGetPartnerInsights where
  arbitrary = sized genCraCheckReportPartnerInsightsGetPartnerInsights

genCraCheckReportPartnerInsightsGetPartnerInsights :: Int -> Gen CraCheckReportPartnerInsightsGetPartnerInsights
genCraCheckReportPartnerInsightsGetPartnerInsights n =
  CraCheckReportPartnerInsightsGetPartnerInsights
    <$> arbitraryReducedMaybe n -- craCheckReportPartnerInsightsGetPartnerInsightsFico :: Maybe CraPartnerInsightsFicoInput
    <*> arbitraryReducedMaybe n -- craCheckReportPartnerInsightsGetPartnerInsightsPrismVersions :: Maybe PrismVersions
  
instance Arbitrary CraCheckReportPartnerInsightsGetRequest where
  arbitrary = sized genCraCheckReportPartnerInsightsGetRequest

genCraCheckReportPartnerInsightsGetRequest :: Int -> Gen CraCheckReportPartnerInsightsGetRequest
genCraCheckReportPartnerInsightsGetRequest n =
  CraCheckReportPartnerInsightsGetRequest
    <$> arbitraryReducedMaybe n -- craCheckReportPartnerInsightsGetRequestClientId :: Maybe Text
    <*> arbitraryReducedMaybe n -- craCheckReportPartnerInsightsGetRequestOptions :: Maybe CraCheckReportPartnerInsightsGetOptions
    <*> arbitraryReducedMaybe n -- craCheckReportPartnerInsightsGetRequestPartnerInsights :: Maybe CraCheckReportPartnerInsightsGetPartnerInsights
    <*> arbitraryReducedMaybe n -- craCheckReportPartnerInsightsGetRequestSecret :: Maybe Text
    <*> arbitraryReducedMaybe n -- craCheckReportPartnerInsightsGetRequestThirdPartyUserToken :: Maybe Text
    <*> arbitraryReducedMaybe n -- craCheckReportPartnerInsightsGetRequestUserId :: Maybe Text
    <*> arbitraryReducedMaybe n -- craCheckReportPartnerInsightsGetRequestUserTier :: Maybe CraUserTier
    <*> arbitraryReducedMaybe n -- craCheckReportPartnerInsightsGetRequestUserToken :: Maybe Text
  
instance Arbitrary CraCheckReportPartnerInsightsGetResponse where
  arbitrary = sized genCraCheckReportPartnerInsightsGetResponse

genCraCheckReportPartnerInsightsGetResponse :: Int -> Gen CraCheckReportPartnerInsightsGetResponse
genCraCheckReportPartnerInsightsGetResponse n =
  CraCheckReportPartnerInsightsGetResponse
    <$> arbitraryReducedMaybe n -- craCheckReportPartnerInsightsGetResponseReport :: Maybe CraPartnerInsights
    <*> arbitrary -- craCheckReportPartnerInsightsGetResponseRequestId :: Text
    <*> arbitraryReducedMaybe n -- craCheckReportPartnerInsightsGetResponseWarnings :: Maybe [CheckReportWarning]
  
instance Arbitrary CraCurrentModeledIncome where
  arbitrary = sized genCraCurrentModeledIncome

genCraCurrentModeledIncome :: Int -> Gen CraCurrentModeledIncome
genCraCurrentModeledIncome n =
  CraCurrentModeledIncome
    <$> arbitraryReduced n -- craCurrentModeledIncomeAnnual :: CraAnnualIncomeValues
    <*> arbitraryReduced n -- craCurrentModeledIncomeMonthly :: CraMonthlyIncomeValues
  
instance Arbitrary CraIncomeCategory where
  arbitrary = sized genCraIncomeCategory

genCraIncomeCategory :: Int -> Gen CraIncomeCategory
genCraIncomeCategory n =
  CraIncomeCategory
    <$> arbitrary -- craIncomeCategoryPrimary :: Text
    <*> arbitrary -- craIncomeCategorySecondary :: Text
  
instance Arbitrary CraIncomeInsights where
  arbitrary = sized genCraIncomeInsights

genCraIncomeInsights :: Int -> Gen CraIncomeInsights
genCraIncomeInsights n =
  CraIncomeInsights
    <$> arbitraryReducedMaybe n -- craIncomeInsightsBankIncomeSummary :: Maybe CraBankIncomeSummary
    <*> arbitraryReducedMaybe n -- craIncomeInsightsClientReportId :: Maybe Text
    <*> arbitraryReducedMaybe n -- craIncomeInsightsDaysRequested :: Maybe Int
    <*> arbitraryReducedMaybe n -- craIncomeInsightsGeneratedTime :: Maybe DateTime
    <*> arbitraryReduced n -- craIncomeInsightsIncomeStreams :: [CraIncomeStream]
    <*> arbitraryReducedMaybe n -- craIncomeInsightsItems :: Maybe [CraBankIncomeItem]
    <*> arbitraryReducedMaybe n -- craIncomeInsightsReportId :: Maybe Text
    <*> arbitraryReducedMaybe n -- craIncomeInsightsUserSummary :: Maybe CraIncomeInsightsUserSummary
    <*> arbitraryReducedMaybe n -- craIncomeInsightsWarnings :: Maybe [CraBankIncomeWarning]
  
instance Arbitrary CraIncomeInsightsUserSummary where
  arbitrary = sized genCraIncomeInsightsUserSummary

genCraIncomeInsightsUserSummary :: Int -> Gen CraIncomeInsightsUserSummary
genCraIncomeInsightsUserSummary n =
  CraIncomeInsightsUserSummary
    <$> arbitraryReduced n -- craIncomeInsightsUserSummaryIncomeMetrics :: [CraIncomeMetrics]
  
instance Arbitrary CraIncomeMetrics where
  arbitrary = sized genCraIncomeMetrics

genCraIncomeMetrics :: Int -> Gen CraIncomeMetrics
genCraIncomeMetrics n =
  CraIncomeMetrics
    <$> arbitraryReduced n -- craIncomeMetricsCurrent :: CraCurrentModeledIncome
    <*> arbitrary -- craIncomeMetricsIsoCurrencyCode :: Text
    <*> arbitraryReduced n -- craIncomeMetricsProjected :: CraProjectedModeledIncome
    <*> arbitrary -- craIncomeMetricsUnofficialCurrencyCode :: Text
  
instance Arbitrary CraIncomeNextPayment where
  arbitrary = sized genCraIncomeNextPayment

genCraIncomeNextPayment :: Int -> Gen CraIncomeNextPayment
genCraIncomeNextPayment n =
  CraIncomeNextPayment
    <$> arbitraryReduced n -- craIncomeNextPaymentDate :: Date
  
instance Arbitrary CraIncomeStream where
  arbitrary = sized genCraIncomeStream

genCraIncomeStream :: Int -> Gen CraIncomeStream
genCraIncomeStream n =
  CraIncomeStream
    <$> arbitrary -- craIncomeStreamDescription :: Text
    <*> arbitraryReduced n -- craIncomeStreamEndDate :: Date
    <*> arbitraryReduced n -- craIncomeStreamIncomeMetrics :: CraIncomeMetrics
    <*> arbitrary -- craIncomeStreamIncomeStreamId :: Text
    <*> arbitraryReduced n -- craIncomeStreamInsights :: CraIncomeStreamInsights
    <*> arbitraryReduced n -- craIncomeStreamStartDate :: Date
    <*> arbitraryReduced n -- craIncomeStreamTransactions :: [CraIncomeTransaction]
  
instance Arbitrary CraIncomeStreamInsights where
  arbitrary = sized genCraIncomeStreamInsights

genCraIncomeStreamInsights :: Int -> Gen CraIncomeStreamInsights
genCraIncomeStreamInsights n =
  CraIncomeStreamInsights
    <$> arbitraryReduced n -- craIncomeStreamInsightsIncomeCategory :: CraIncomeCategory
    <*> arbitraryReduced n -- craIncomeStreamInsightsIncomeProvider :: CraBankIncomeIncomeProvider
    <*> arbitraryReduced n -- craIncomeStreamInsightsNextPayment :: CraIncomeNextPayment
    <*> arbitraryReduced n -- craIncomeStreamInsightsPayFrequency :: CreditBankIncomePayFrequency
    <*> arbitraryReduced n -- craIncomeStreamInsightsStatus :: CraBankIncomeStatus
  
instance Arbitrary CraIncomeTransaction where
  arbitrary = sized genCraIncomeTransaction

genCraIncomeTransaction :: Int -> Gen CraIncomeTransaction
genCraIncomeTransaction n =
  CraIncomeTransaction
    <$> arbitrary -- craIncomeTransactionAccountId :: Text
    <*> arbitrary -- craIncomeTransactionAmount :: Double
    <*> arbitraryReduced n -- craIncomeTransactionDate :: Date
    <*> arbitrary -- craIncomeTransactionIsoCurrencyCode :: Text
    <*> arbitrary -- craIncomeTransactionItemId :: Text
    <*> arbitrary -- craIncomeTransactionOriginalDescription :: Text
    <*> arbitraryReduced n -- craIncomeTransactionOutlier :: CraIncomeTransactionOutlier
    <*> arbitrary -- craIncomeTransactionTransactionId :: Text
    <*> arbitrary -- craIncomeTransactionUnofficialCurrencyCode :: Text
  
instance Arbitrary CraIncomeTransactionOutlier where
  arbitrary = sized genCraIncomeTransactionOutlier

genCraIncomeTransactionOutlier :: Int -> Gen CraIncomeTransactionOutlier
genCraIncomeTransactionOutlier n =
  CraIncomeTransactionOutlier
    <$> arbitraryReducedMaybe n -- craIncomeTransactionOutlierAmount :: Maybe Double
    <*> arbitrary -- craIncomeTransactionOutlierIsOutlier :: Bool
  
instance Arbitrary CraMonthlyIncomeValues where
  arbitrary = sized genCraMonthlyIncomeValues

genCraMonthlyIncomeValues :: Int -> Gen CraMonthlyIncomeValues
genCraMonthlyIncomeValues n =
  CraMonthlyIncomeValues
    <$> arbitrary -- craMonthlyIncomeValuesGrossIncome :: Double
    <*> arbitrary -- craMonthlyIncomeValuesNetIncome :: Double
  
instance Arbitrary CraPartnerInsights where
  arbitrary = sized genCraPartnerInsights

genCraPartnerInsights :: Int -> Gen CraPartnerInsights
genCraPartnerInsights n =
  CraPartnerInsights
    <$> arbitraryReducedMaybe n -- craPartnerInsightsClientReportId :: Maybe Text
    <*> arbitraryReducedMaybe n -- craPartnerInsightsFico :: Maybe CraPartnerInsightsFicoResults
    <*> arbitraryReducedMaybe n -- craPartnerInsightsGeneratedTime :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- craPartnerInsightsItems :: Maybe [CraPartnerInsightsItem]
    <*> arbitraryReducedMaybe n -- craPartnerInsightsPrism :: Maybe CraPartnerInsightsPrism
    <*> arbitraryReducedMaybe n -- craPartnerInsightsReportId :: Maybe Text
  
instance Arbitrary CraPartnerInsightsBaseFicoScore where
  arbitrary = sized genCraPartnerInsightsBaseFicoScore

genCraPartnerInsightsBaseFicoScore :: Int -> Gen CraPartnerInsightsBaseFicoScore
genCraPartnerInsightsBaseFicoScore n =
  CraPartnerInsightsBaseFicoScore
    <$> arbitraryReduced n -- craPartnerInsightsBaseFicoScoreBaseFicoScoreVersion :: CraPartnerInsightsBaseFicoScoreVersion
    <*> arbitraryReduced n -- craPartnerInsightsBaseFicoScoreBureau :: CraPartnerInsightsBureau
    <*> arbitraryReducedMaybe n -- craPartnerInsightsBaseFicoScoreDidInquiriesAdverselyAffectScore :: Maybe Bool
    <*> arbitraryReducedMaybe n -- craPartnerInsightsBaseFicoScoreReasonCode1 :: Maybe Text
    <*> arbitraryReducedMaybe n -- craPartnerInsightsBaseFicoScoreReasonCode2 :: Maybe Text
    <*> arbitraryReducedMaybe n -- craPartnerInsightsBaseFicoScoreReasonCode3 :: Maybe Text
    <*> arbitraryReducedMaybe n -- craPartnerInsightsBaseFicoScoreReasonCode4 :: Maybe Text
    <*> arbitraryReducedMaybe n -- craPartnerInsightsBaseFicoScoreReasonCodes :: Maybe [Text]
    <*> arbitrary -- craPartnerInsightsBaseFicoScoreScore :: Int
  
instance Arbitrary CraPartnerInsightsFicoInput where
  arbitrary = sized genCraPartnerInsightsFicoInput

genCraPartnerInsightsFicoInput :: Int -> Gen CraPartnerInsightsFicoInput
genCraPartnerInsightsFicoInput n =
  CraPartnerInsightsFicoInput
    <$> arbitrary -- craPartnerInsightsFicoInputFicoLenderId :: Text
    <*> arbitrary -- craPartnerInsightsFicoInputLenderApplicationId :: Text
    <*> arbitraryReduced n -- craPartnerInsightsFicoInputUltraficoScoreRequests :: [CraPartnerInsightsUltraFicoScoreRequest]
  
instance Arbitrary CraPartnerInsightsFicoReportCharacteristics where
  arbitrary = sized genCraPartnerInsightsFicoReportCharacteristics

genCraPartnerInsightsFicoReportCharacteristics :: Int -> Gen CraPartnerInsightsFicoReportCharacteristics
genCraPartnerInsightsFicoReportCharacteristics n =
  CraPartnerInsightsFicoReportCharacteristics
    <$> arbitraryReducedMaybe n -- craPartnerInsightsFicoReportCharacteristicsAvgDailyBalanceOver12Months :: Maybe Double
    <*> arbitraryReducedMaybe n -- craPartnerInsightsFicoReportCharacteristicsAvgDailyBalanceOver1Month :: Maybe Double
    <*> arbitraryReducedMaybe n -- craPartnerInsightsFicoReportCharacteristicsAvgDailyBalanceOver3Months :: Maybe Double
    <*> arbitraryReducedMaybe n -- craPartnerInsightsFicoReportCharacteristicsAvgDailyBalanceOver6Months :: Maybe Double
    <*> arbitraryReducedMaybe n -- craPartnerInsightsFicoReportCharacteristicsDaysSinceEarliestTx :: Maybe Int
    <*> arbitraryReducedMaybe n -- craPartnerInsightsFicoReportCharacteristicsDaysSinceMostRecentInsufficientFundsFeeDebitTx :: Maybe Int
    <*> arbitraryReducedMaybe n -- craPartnerInsightsFicoReportCharacteristicsDaysSinceMostRecentNegativeEndingBalance :: Maybe Int
    <*> arbitraryReducedMaybe n -- craPartnerInsightsFicoReportCharacteristicsDaysSinceMostRecentTx :: Maybe Int
    <*> arbitraryReducedMaybe n -- craPartnerInsightsFicoReportCharacteristicsDaysWithTxOver12Months :: Maybe Int
    <*> arbitraryReducedMaybe n -- craPartnerInsightsFicoReportCharacteristicsDaysWithTxOver1Month :: Maybe Int
    <*> arbitraryReducedMaybe n -- craPartnerInsightsFicoReportCharacteristicsDaysWithTxOver3Months :: Maybe Int
    <*> arbitraryReducedMaybe n -- craPartnerInsightsFicoReportCharacteristicsDaysWithTxOver6Months :: Maybe Int
    <*> arbitraryReducedMaybe n -- craPartnerInsightsFicoReportCharacteristicsNumAccounts :: Maybe Int
    <*> arbitraryReducedMaybe n -- craPartnerInsightsFicoReportCharacteristicsNumCheckingAccounts :: Maybe Int
    <*> arbitraryReducedMaybe n -- craPartnerInsightsFicoReportCharacteristicsNumMoneyMarketAccounts :: Maybe Int
    <*> arbitraryReducedMaybe n -- craPartnerInsightsFicoReportCharacteristicsNumSavingsAccounts :: Maybe Int
    <*> arbitraryReducedMaybe n -- craPartnerInsightsFicoReportCharacteristicsTotCurrentBalances :: Maybe Double
    <*> arbitraryReducedMaybe n -- craPartnerInsightsFicoReportCharacteristicsTotNumberDaysWithNegativeBalanceOver12Months :: Maybe Int
    <*> arbitraryReducedMaybe n -- craPartnerInsightsFicoReportCharacteristicsTotNumberDaysWithNegativeBalanceOver1Month :: Maybe Int
    <*> arbitraryReducedMaybe n -- craPartnerInsightsFicoReportCharacteristicsTotNumberDaysWithNegativeBalanceOver3Months :: Maybe Int
    <*> arbitraryReducedMaybe n -- craPartnerInsightsFicoReportCharacteristicsTotNumberDaysWithNegativeBalanceOver6Months :: Maybe Int
  
instance Arbitrary CraPartnerInsightsFicoResults where
  arbitrary = sized genCraPartnerInsightsFicoResults

genCraPartnerInsightsFicoResults :: Int -> Gen CraPartnerInsightsFicoResults
genCraPartnerInsightsFicoResults n =
  CraPartnerInsightsFicoResults
    <$> arbitrary -- craPartnerInsightsFicoResultsLenderApplicationId :: Text
    <*> arbitraryReducedMaybe n -- craPartnerInsightsFicoResultsReportCharacteristics :: Maybe CraPartnerInsightsFicoReportCharacteristics
    <*> arbitraryReduced n -- craPartnerInsightsFicoResultsUltraficoScoreResults :: [CraPartnerInsightsUltraFicoScoreResult]
  
instance Arbitrary CraPartnerInsightsItem where
  arbitrary = sized genCraPartnerInsightsItem

genCraPartnerInsightsItem :: Int -> Gen CraPartnerInsightsItem
genCraPartnerInsightsItem n =
  CraPartnerInsightsItem
    <$> arbitraryReducedMaybe n -- craPartnerInsightsItemAccounts :: Maybe [CraPartnerInsightsItemAccount]
    <*> arbitraryReducedMaybe n -- craPartnerInsightsItemInstitutionId :: Maybe Text
    <*> arbitraryReducedMaybe n -- craPartnerInsightsItemInstitutionName :: Maybe Text
    <*> arbitraryReducedMaybe n -- craPartnerInsightsItemItemId :: Maybe Text
  
instance Arbitrary CraPartnerInsightsItemAccount where
  arbitrary = sized genCraPartnerInsightsItemAccount

genCraPartnerInsightsItemAccount :: Int -> Gen CraPartnerInsightsItemAccount
genCraPartnerInsightsItemAccount n =
  CraPartnerInsightsItemAccount
    <$> arbitraryReducedMaybe n -- craPartnerInsightsItemAccountAccountId :: Maybe Text
    <*> arbitrary -- craPartnerInsightsItemAccountMask :: Text
    <*> arbitraryReduced n -- craPartnerInsightsItemAccountMetadata :: CraPartnerInsightsItemAccountMetadata
    <*> arbitrary -- craPartnerInsightsItemAccountName :: Text
    <*> arbitrary -- craPartnerInsightsItemAccountOfficialName :: Text
    <*> arbitraryReduced n -- craPartnerInsightsItemAccountOwners :: [Owner]
    <*> arbitraryReduced n -- craPartnerInsightsItemAccountSubtype :: DepositoryAccountSubtype
    <*> arbitraryReduced n -- craPartnerInsightsItemAccountType :: CreditBankIncomeAccountType
  
instance Arbitrary CraPartnerInsightsItemAccountMetadata where
  arbitrary = sized genCraPartnerInsightsItemAccountMetadata

genCraPartnerInsightsItemAccountMetadata :: Int -> Gen CraPartnerInsightsItemAccountMetadata
genCraPartnerInsightsItemAccountMetadata n =
  CraPartnerInsightsItemAccountMetadata
    <$> arbitraryReduced n -- craPartnerInsightsItemAccountMetadataEndDate :: Date
    <*> arbitraryReduced n -- craPartnerInsightsItemAccountMetadataStartDate :: Date
  
instance Arbitrary CraPartnerInsightsPrism where
  arbitrary = sized genCraPartnerInsightsPrism

genCraPartnerInsightsPrism :: Int -> Gen CraPartnerInsightsPrism
genCraPartnerInsightsPrism n =
  CraPartnerInsightsPrism
    <$> arbitraryReducedMaybe n -- craPartnerInsightsPrismCashScore :: Maybe PrismCashScore
    <*> arbitraryReducedMaybe n -- craPartnerInsightsPrismDetect :: Maybe PrismDetect
    <*> arbitraryReducedMaybe n -- craPartnerInsightsPrismExtend :: Maybe PrismExtend
    <*> arbitraryReducedMaybe n -- craPartnerInsightsPrismFirstDetect :: Maybe PrismFirstDetect
    <*> arbitraryReducedMaybe n -- craPartnerInsightsPrismInsights :: Maybe PrismInsights
    <*> arbitrary -- craPartnerInsightsPrismStatus :: Text
  
instance Arbitrary CraPartnerInsightsUltraFicoScore where
  arbitrary = sized genCraPartnerInsightsUltraFicoScore

genCraPartnerInsightsUltraFicoScore :: Int -> Gen CraPartnerInsightsUltraFicoScore
genCraPartnerInsightsUltraFicoScore n =
  CraPartnerInsightsUltraFicoScore
    <$> arbitraryReducedMaybe n -- craPartnerInsightsUltraFicoScoreDidInquiriesAdverselyAffectScore :: Maybe Bool
    <*> arbitraryReducedMaybe n -- craPartnerInsightsUltraFicoScoreNegativeReasonCodes :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- craPartnerInsightsUltraFicoScorePositiveReasonCode1 :: Maybe Text
    <*> arbitraryReducedMaybe n -- craPartnerInsightsUltraFicoScorePositiveReasonCode2 :: Maybe Text
    <*> arbitraryReducedMaybe n -- craPartnerInsightsUltraFicoScorePositiveReasonCode3 :: Maybe Text
    <*> arbitraryReducedMaybe n -- craPartnerInsightsUltraFicoScorePositiveReasonCode4 :: Maybe Text
    <*> arbitraryReducedMaybe n -- craPartnerInsightsUltraFicoScorePositiveReasonCodes :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- craPartnerInsightsUltraFicoScoreReasonCode1 :: Maybe Text
    <*> arbitraryReducedMaybe n -- craPartnerInsightsUltraFicoScoreReasonCode2 :: Maybe Text
    <*> arbitraryReducedMaybe n -- craPartnerInsightsUltraFicoScoreReasonCode3 :: Maybe Text
    <*> arbitraryReducedMaybe n -- craPartnerInsightsUltraFicoScoreReasonCode4 :: Maybe Text
    <*> arbitrary -- craPartnerInsightsUltraFicoScoreScore :: Int
    <*> arbitraryReduced n -- craPartnerInsightsUltraFicoScoreUltraficoScoreVersion :: CraPartnerInsightsUltraFicoScoreVersion
  
instance Arbitrary CraPartnerInsightsUltraFicoScoreRequest where
  arbitrary = sized genCraPartnerInsightsUltraFicoScoreRequest

genCraPartnerInsightsUltraFicoScoreRequest :: Int -> Gen CraPartnerInsightsUltraFicoScoreRequest
genCraPartnerInsightsUltraFicoScoreRequest n =
  CraPartnerInsightsUltraFicoScoreRequest
    <$> arbitraryReduced n -- craPartnerInsightsUltraFicoScoreRequestBaseFicoScore :: CraPartnerInsightsBaseFicoScore
    <*> arbitraryReducedMaybe n -- craPartnerInsightsUltraFicoScoreRequestFicoScoringRequestId :: Maybe Text
    <*> arbitraryReducedMaybe n -- craPartnerInsightsUltraFicoScoreRequestRequestCorrelationId :: Maybe Text
    <*> arbitraryReduced n -- craPartnerInsightsUltraFicoScoreRequestUltraficoScoreVersion :: CraPartnerInsightsUltraFicoScoreVersion
  
instance Arbitrary CraPartnerInsightsUltraFicoScoreResult where
  arbitrary = sized genCraPartnerInsightsUltraFicoScoreResult

genCraPartnerInsightsUltraFicoScoreResult :: Int -> Gen CraPartnerInsightsUltraFicoScoreResult
genCraPartnerInsightsUltraFicoScoreResult n =
  CraPartnerInsightsUltraFicoScoreResult
    <$> arbitraryReducedMaybe n -- craPartnerInsightsUltraFicoScoreResultErrorReason :: Maybe Text
    <*> arbitraryReducedMaybe n -- craPartnerInsightsUltraFicoScoreResultExclusionCode :: Maybe Text
    <*> arbitraryReducedMaybe n -- craPartnerInsightsUltraFicoScoreResultFicoScoringRequestId :: Maybe Text
    <*> arbitraryReducedMaybe n -- craPartnerInsightsUltraFicoScoreResultRequestCorrelationId :: Maybe Text
    <*> arbitraryReducedMaybe n -- craPartnerInsightsUltraFicoScoreResultUltraficoScore :: Maybe CraPartnerInsightsUltraFicoScore
  
instance Arbitrary CraPredictionInterval where
  arbitrary = sized genCraPredictionInterval

genCraPredictionInterval :: Int -> Gen CraPredictionInterval
genCraPredictionInterval n =
  CraPredictionInterval
    <$> arbitraryReducedMaybe n -- craPredictionIntervalLowerBound :: Maybe Double
    <*> arbitraryReducedMaybe n -- craPredictionIntervalProbability :: Maybe Double
    <*> arbitraryReducedMaybe n -- craPredictionIntervalUpperBound :: Maybe Double
  
instance Arbitrary CraProjectedModeledIncome where
  arbitrary = sized genCraProjectedModeledIncome

genCraProjectedModeledIncome :: Int -> Gen CraProjectedModeledIncome
genCraProjectedModeledIncome n =
  CraProjectedModeledIncome
    <$> arbitraryReduced n -- craProjectedModeledIncomeAnnual :: CraAnnualIncomeValues
    <*> arbitraryReduced n -- craProjectedModeledIncomeMonthly :: CraMonthlyIncomeValues
  
instance Arbitrary CreditAmountWithCurrency where
  arbitrary = sized genCreditAmountWithCurrency

genCreditAmountWithCurrency :: Int -> Gen CreditAmountWithCurrency
genCreditAmountWithCurrency n =
  CreditAmountWithCurrency
    <$> arbitrary -- creditAmountWithCurrencyAmount :: Double
    <*> arbitrary -- creditAmountWithCurrencyIsoCurrencyCode :: Text
    <*> arbitrary -- creditAmountWithCurrencyUnofficialCurrencyCode :: Text
  
instance Arbitrary CreditCategory where
  arbitrary = sized genCreditCategory

genCreditCategory :: Int -> Gen CreditCategory
genCreditCategory n =
  CreditCategory
    <$> arbitrary -- creditCategoryDetailed :: Text
    <*> arbitrary -- creditCategoryPrimary :: Text
  
instance Arbitrary CustomerInitiatedReturnRisk where
  arbitrary = sized genCustomerInitiatedReturnRisk

genCustomerInitiatedReturnRisk :: Int -> Gen CustomerInitiatedReturnRisk
genCustomerInitiatedReturnRisk n =
  CustomerInitiatedReturnRisk
    <$> arbitrary -- customerInitiatedReturnRiskRiskTier :: Int
    <*> arbitrary -- customerInitiatedReturnRiskScore :: Int
  
instance Arbitrary IncomeInsightsFilter where
  arbitrary = sized genIncomeInsightsFilter

genIncomeInsightsFilter :: Int -> Gen IncomeInsightsFilter
genIncomeInsightsFilter n =
  IncomeInsightsFilter
    <$> arbitraryReducedMaybe n -- incomeInsightsFilterExcludedCategories :: Maybe [Text]
    <*> arbitrary -- incomeInsightsFilterIncludedCategories :: [Text]
  
instance Arbitrary PlaidError where
  arbitrary = sized genPlaidError

genPlaidError :: Int -> Gen PlaidError
genPlaidError n =
  PlaidError
    <$> arbitraryReducedMaybe n -- plaidErrorCauses :: Maybe [A.Value]
    <*> arbitrary -- plaidErrorDisplayMessage :: Text
    <*> arbitraryReducedMaybe n -- plaidErrorDocumentationUrl :: Maybe Text
    <*> arbitrary -- plaidErrorErrorCode :: Text
    <*> arbitraryReducedMaybe n -- plaidErrorErrorCodeReason :: Maybe Text
    <*> arbitrary -- plaidErrorErrorMessage :: Text
    <*> arbitraryReduced n -- plaidErrorErrorType :: PlaidErrorType
    <*> arbitraryReducedMaybe n -- plaidErrorProvidedAccountSubtypes :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- plaidErrorRequestId :: Maybe Text
    <*> arbitraryReducedMaybe n -- plaidErrorRequiredAccountSubtypes :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- plaidErrorStatus :: Maybe Int
    <*> arbitraryReducedMaybe n -- plaidErrorSuggestedAction :: Maybe Text
  
instance Arbitrary PrismCashScore where
  arbitrary = sized genPrismCashScore

genPrismCashScore :: Int -> Gen PrismCashScore
genPrismCashScore n =
  PrismCashScore
    <$> arbitraryReducedMaybe n -- prismCashScoreErrorReason :: Maybe Text
    <*> arbitraryReducedMaybe n -- prismCashScoreMetadata :: Maybe PrismCashScoreMetadata
    <*> arbitraryReducedMaybe n -- prismCashScoreModelVersion :: Maybe Text
    <*> arbitraryReducedMaybe n -- prismCashScoreReasonCodes :: Maybe [Text]
    <*> arbitrary -- prismCashScoreScore :: Int
    <*> arbitrary -- prismCashScoreVersion :: Int
  
instance Arbitrary PrismCashScoreMetadata where
  arbitrary = sized genPrismCashScoreMetadata

genPrismCashScoreMetadata :: Int -> Gen PrismCashScoreMetadata
genPrismCashScoreMetadata n =
  PrismCashScoreMetadata
    <$> arbitrary -- prismCashScoreMetadataL1mCreditValueCnt :: Int
    <*> arbitrary -- prismCashScoreMetadataL1mDebitValueCnt :: Int
    <*> arbitrary -- prismCashScoreMetadataMaxAge :: Int
    <*> arbitrary -- prismCashScoreMetadataMaxAgeCredit :: Int
    <*> arbitrary -- prismCashScoreMetadataMaxAgeDebit :: Int
    <*> arbitrary -- prismCashScoreMetadataMinAge :: Int
    <*> arbitrary -- prismCashScoreMetadataMinAgeCredit :: Int
    <*> arbitrary -- prismCashScoreMetadataMinAgeDebit :: Int
    <*> arbitrary -- prismCashScoreMetadataNumTrxnCredit :: Int
    <*> arbitrary -- prismCashScoreMetadataNumTrxnDebit :: Int
  
instance Arbitrary PrismDetect where
  arbitrary = sized genPrismDetect

genPrismDetect :: Int -> Gen PrismDetect
genPrismDetect n =
  PrismDetect
    <$> arbitraryReducedMaybe n -- prismDetectErrorReason :: Maybe Text
    <*> arbitraryReducedMaybe n -- prismDetectMetadata :: Maybe PrismCashScoreMetadata
    <*> arbitrary -- prismDetectModelVersion :: Text
    <*> arbitraryReducedMaybe n -- prismDetectReasonCodes :: Maybe [Text]
    <*> arbitrary -- prismDetectScore :: Int
  
instance Arbitrary PrismExtend where
  arbitrary = sized genPrismExtend

genPrismExtend :: Int -> Gen PrismExtend
genPrismExtend n =
  PrismExtend
    <$> arbitraryReducedMaybe n -- prismExtendErrorReason :: Maybe Text
    <*> arbitraryReducedMaybe n -- prismExtendMetadata :: Maybe PrismCashScoreMetadata
    <*> arbitrary -- prismExtendModelVersion :: Text
    <*> arbitraryReducedMaybe n -- prismExtendReasonCodes :: Maybe [Text]
    <*> arbitrary -- prismExtendScore :: Int
  
instance Arbitrary PrismFirstDetect where
  arbitrary = sized genPrismFirstDetect

genPrismFirstDetect :: Int -> Gen PrismFirstDetect
genPrismFirstDetect n =
  PrismFirstDetect
    <$> arbitraryReducedMaybe n -- prismFirstDetectErrorReason :: Maybe Text
    <*> arbitraryReducedMaybe n -- prismFirstDetectMetadata :: Maybe PrismCashScoreMetadata
    <*> arbitraryReducedMaybe n -- prismFirstDetectModelVersion :: Maybe Text
    <*> arbitraryReducedMaybe n -- prismFirstDetectReasonCodes :: Maybe [Text]
    <*> arbitrary -- prismFirstDetectScore :: Int
    <*> arbitrary -- prismFirstDetectVersion :: Int
  
instance Arbitrary PrismInsights where
  arbitrary = sized genPrismInsights

genPrismInsights :: Int -> Gen PrismInsights
genPrismInsights n =
  PrismInsights
    <$> arbitraryReducedMaybe n -- prismInsightsErrorReason :: Maybe Text
    <*> arbitraryReducedMaybe n -- prismInsightsModelVersion :: Maybe Text
    <*> arbitraryReducedMaybeValue n -- prismInsightsResult :: Maybe A.Value
    <*> arbitrary -- prismInsightsVersion :: Int
  
instance Arbitrary PrismVersions where
  arbitrary = sized genPrismVersions

genPrismVersions :: Int -> Gen PrismVersions
genPrismVersions n =
  PrismVersions
    <$> arbitraryReducedMaybe n -- prismVersionsCashscore :: Maybe PrismCashScoreVersion
    <*> arbitraryReducedMaybe n -- prismVersionsDetect :: Maybe PrismDetectVersion
    <*> arbitraryReducedMaybe n -- prismVersionsExtend :: Maybe PrismExtendVersion
    <*> arbitraryReducedMaybe n -- prismVersionsFirstdetect :: Maybe PrismFirstDetectVersion
    <*> arbitraryReducedMaybe n -- prismVersionsInsights :: Maybe PrismInsightsVersion
  
instance Arbitrary PrismVersionsDeprecated where
  arbitrary = sized genPrismVersionsDeprecated

genPrismVersionsDeprecated :: Int -> Gen PrismVersionsDeprecated
genPrismVersionsDeprecated n =
  PrismVersionsDeprecated
    <$> arbitraryReducedMaybe n -- prismVersionsDeprecatedCashscore :: Maybe PrismCashScoreVersion
    <*> arbitraryReducedMaybe n -- prismVersionsDeprecatedDetect :: Maybe PrismDetectVersion
    <*> arbitraryReducedMaybe n -- prismVersionsDeprecatedExtend :: Maybe PrismExtendVersion
    <*> arbitraryReducedMaybe n -- prismVersionsDeprecatedFirstdetect :: Maybe PrismFirstDetectVersion
    <*> arbitraryReducedMaybe n -- prismVersionsDeprecatedInsights :: Maybe PrismInsightsVersion
  
instance Arbitrary RiskProfile where
  arbitrary = sized genRiskProfile

genRiskProfile :: Int -> Gen RiskProfile
genRiskProfile n =
  RiskProfile
    <$> arbitraryReducedMaybe n -- riskProfileKey :: Maybe Text
    <*> arbitraryReducedMaybe n -- riskProfileOutcome :: Maybe Text
  
instance Arbitrary RuleDetails where
  arbitrary = sized genRuleDetails

genRuleDetails :: Int -> Gen RuleDetails
genRuleDetails n =
  RuleDetails
    <$> arbitraryReducedMaybe n -- ruleDetailsCustomActionKey :: Maybe Text
    <*> arbitraryReducedMaybe n -- ruleDetailsInternalNote :: Maybe Text
  
instance Arbitrary Ruleset where
  arbitrary = sized genRuleset

genRuleset :: Int -> Gen Ruleset
genRuleset n =
  Ruleset
    <$> arbitraryReducedMaybe n -- rulesetOutcome :: Maybe Text
    <*> arbitraryReduced n -- rulesetResult :: RuleResult
    <*> arbitraryReducedMaybe n -- rulesetRulesetKey :: Maybe Text
    <*> arbitraryReducedMaybe n -- rulesetTriggeredRuleDetails :: Maybe RuleDetails
  
instance Arbitrary SignalAddressData where
  arbitrary = sized genSignalAddressData

genSignalAddressData :: Int -> Gen SignalAddressData
genSignalAddressData n =
  SignalAddressData
    <$> arbitraryReducedMaybe n -- signalAddressDataCity :: Maybe Text
    <*> arbitraryReducedMaybe n -- signalAddressDataCountry :: Maybe Text
    <*> arbitraryReducedMaybe n -- signalAddressDataPostalCode :: Maybe Text
    <*> arbitraryReducedMaybe n -- signalAddressDataRegion :: Maybe Text
    <*> arbitraryReducedMaybe n -- signalAddressDataStreet :: Maybe Text
  
instance Arbitrary SignalDecisionReportRequest where
  arbitrary = sized genSignalDecisionReportRequest

genSignalDecisionReportRequest :: Int -> Gen SignalDecisionReportRequest
genSignalDecisionReportRequest n =
  SignalDecisionReportRequest
    <$> arbitraryReducedMaybe n -- signalDecisionReportRequestAmountInstantlyAvailable :: Maybe Double
    <*> arbitraryReducedMaybe n -- signalDecisionReportRequestClientId :: Maybe Text
    <*> arbitrary -- signalDecisionReportRequestClientTransactionId :: Text
    <*> arbitraryReducedMaybe n -- signalDecisionReportRequestDaysFundsOnHold :: Maybe Int
    <*> arbitraryReducedMaybe n -- signalDecisionReportRequestDecisionOutcome :: Maybe SignalDecisionOutcome
    <*> arbitrary -- signalDecisionReportRequestInitiated :: Bool
    <*> arbitraryReducedMaybe n -- signalDecisionReportRequestPaymentMethod :: Maybe SignalPaymentMethod
    <*> arbitraryReducedMaybe n -- signalDecisionReportRequestSecret :: Maybe Text
    <*> arbitraryReducedMaybe n -- signalDecisionReportRequestSubmittedAt :: Maybe DateTime
  
instance Arbitrary SignalDecisionReportResponse where
  arbitrary = sized genSignalDecisionReportResponse

genSignalDecisionReportResponse :: Int -> Gen SignalDecisionReportResponse
genSignalDecisionReportResponse n =
  SignalDecisionReportResponse
    <$> arbitrary -- signalDecisionReportResponseRequestId :: Text
  
instance Arbitrary SignalDevice where
  arbitrary = sized genSignalDevice

genSignalDevice :: Int -> Gen SignalDevice
genSignalDevice n =
  SignalDevice
    <$> arbitraryReducedMaybe n -- signalDeviceIpAddress :: Maybe Text
    <*> arbitraryReducedMaybe n -- signalDeviceUserAgent :: Maybe Text
  
instance Arbitrary SignalEvaluateCoreAttributes where
  arbitrary = sized genSignalEvaluateCoreAttributes

genSignalEvaluateCoreAttributes :: Int -> Gen SignalEvaluateCoreAttributes
genSignalEvaluateCoreAttributes n =
  SignalEvaluateCoreAttributes
    <$> arbitraryReducedMaybe n -- signalEvaluateCoreAttributesAddressChangeCount28d :: Maybe Int
    <*> arbitraryReducedMaybe n -- signalEvaluateCoreAttributesAddressChangeCount90d :: Maybe Int
    <*> arbitraryReducedMaybe n -- signalEvaluateCoreAttributesAvailableBalance :: Maybe Double
    <*> arbitraryReducedMaybe n -- signalEvaluateCoreAttributesBalanceLastUpdated :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- signalEvaluateCoreAttributesBalanceToTransactionAmountRatio :: Maybe Double
    <*> arbitraryReducedMaybe n -- signalEvaluateCoreAttributesCreditTransactionsCount10d :: Maybe Int
    <*> arbitraryReducedMaybe n -- signalEvaluateCoreAttributesCreditTransactionsCount30d :: Maybe Int
    <*> arbitraryReducedMaybe n -- signalEvaluateCoreAttributesCreditTransactionsCount60d :: Maybe Int
    <*> arbitraryReducedMaybe n -- signalEvaluateCoreAttributesCreditTransactionsCount90d :: Maybe Int
    <*> arbitraryReducedMaybe n -- signalEvaluateCoreAttributesCurrentBalance :: Maybe Double
    <*> arbitraryReducedMaybe n -- signalEvaluateCoreAttributesDaysSinceAccountOpening :: Maybe Int
    <*> arbitraryReducedMaybe n -- signalEvaluateCoreAttributesDaysSinceFirstPlaidConnection :: Maybe Int
    <*> arbitraryReducedMaybe n -- signalEvaluateCoreAttributesDaysWithNegativeBalanceCount90d :: Maybe Int
    <*> arbitraryReducedMaybe n -- signalEvaluateCoreAttributesDebitTransactionsCount10d :: Maybe Int
    <*> arbitraryReducedMaybe n -- signalEvaluateCoreAttributesDebitTransactionsCount30d :: Maybe Int
    <*> arbitraryReducedMaybe n -- signalEvaluateCoreAttributesDebitTransactionsCount60d :: Maybe Int
    <*> arbitraryReducedMaybe n -- signalEvaluateCoreAttributesDebitTransactionsCount90d :: Maybe Int
    <*> arbitraryReducedMaybe n -- signalEvaluateCoreAttributesDistinctIpAddressesCount30d :: Maybe Int
    <*> arbitraryReducedMaybe n -- signalEvaluateCoreAttributesDistinctIpAddressesCount3d :: Maybe Int
    <*> arbitraryReducedMaybe n -- signalEvaluateCoreAttributesDistinctIpAddressesCount7d :: Maybe Int
    <*> arbitraryReducedMaybe n -- signalEvaluateCoreAttributesDistinctIpAddressesCount90d :: Maybe Int
    <*> arbitraryReducedMaybe n -- signalEvaluateCoreAttributesDistinctSslTlsConnectionSessionsCount30d :: Maybe Int
    <*> arbitraryReducedMaybe n -- signalEvaluateCoreAttributesDistinctSslTlsConnectionSessionsCount3d :: Maybe Int
    <*> arbitraryReducedMaybe n -- signalEvaluateCoreAttributesDistinctSslTlsConnectionSessionsCount7d :: Maybe Int
    <*> arbitraryReducedMaybe n -- signalEvaluateCoreAttributesDistinctSslTlsConnectionSessionsCount90d :: Maybe Int
    <*> arbitraryReducedMaybe n -- signalEvaluateCoreAttributesDistinctUserAgentsCount30d :: Maybe Int
    <*> arbitraryReducedMaybe n -- signalEvaluateCoreAttributesDistinctUserAgentsCount3d :: Maybe Int
    <*> arbitraryReducedMaybe n -- signalEvaluateCoreAttributesDistinctUserAgentsCount7d :: Maybe Int
    <*> arbitraryReducedMaybe n -- signalEvaluateCoreAttributesDistinctUserAgentsCount90d :: Maybe Int
    <*> arbitraryReducedMaybe n -- signalEvaluateCoreAttributesEmailChangeCount28d :: Maybe Int
    <*> arbitraryReducedMaybe n -- signalEvaluateCoreAttributesEmailChangeCount90d :: Maybe Int
    <*> arbitraryReducedMaybe n -- signalEvaluateCoreAttributesFailedPlaidNonOauthAuthenticationAttemptsCount30d :: Maybe Int
    <*> arbitraryReducedMaybe n -- signalEvaluateCoreAttributesFailedPlaidNonOauthAuthenticationAttemptsCount3d :: Maybe Int
    <*> arbitraryReducedMaybe n -- signalEvaluateCoreAttributesFailedPlaidNonOauthAuthenticationAttemptsCount7d :: Maybe Int
    <*> arbitraryReducedMaybe n -- signalEvaluateCoreAttributesIsAccountClosed :: Maybe Bool
    <*> arbitraryReducedMaybe n -- signalEvaluateCoreAttributesIsAccountFrozenOrRestricted :: Maybe Bool
    <*> arbitraryReducedMaybe n -- signalEvaluateCoreAttributesIsSavingsOrMoneyMarketAccount :: Maybe Bool
    <*> arbitraryReducedMaybe n -- signalEvaluateCoreAttributesNsfOverdraftTransactionsCount30d :: Maybe Int
    <*> arbitraryReducedMaybe n -- signalEvaluateCoreAttributesNsfOverdraftTransactionsCount60d :: Maybe Int
    <*> arbitraryReducedMaybe n -- signalEvaluateCoreAttributesNsfOverdraftTransactionsCount7d :: Maybe Int
    <*> arbitraryReducedMaybe n -- signalEvaluateCoreAttributesNsfOverdraftTransactionsCount90d :: Maybe Int
    <*> arbitraryReducedMaybe n -- signalEvaluateCoreAttributesP10EodBalance30d :: Maybe Double
    <*> arbitraryReducedMaybe n -- signalEvaluateCoreAttributesP10EodBalance31dTo60d :: Maybe Double
    <*> arbitraryReducedMaybe n -- signalEvaluateCoreAttributesP10EodBalance60d :: Maybe Double
    <*> arbitraryReducedMaybe n -- signalEvaluateCoreAttributesP10EodBalance61dTo90d :: Maybe Double
    <*> arbitraryReducedMaybe n -- signalEvaluateCoreAttributesP10EodBalance90d :: Maybe Double
    <*> arbitraryReducedMaybe n -- signalEvaluateCoreAttributesP50CreditTransactionsAmount28d :: Maybe Double
    <*> arbitraryReducedMaybe n -- signalEvaluateCoreAttributesP50DebitTransactionsAmount28d :: Maybe Double
    <*> arbitraryReducedMaybe n -- signalEvaluateCoreAttributesP50EodBalance30d :: Maybe Double
    <*> arbitraryReducedMaybe n -- signalEvaluateCoreAttributesP50EodBalance31dTo60d :: Maybe Double
    <*> arbitraryReducedMaybe n -- signalEvaluateCoreAttributesP50EodBalance60d :: Maybe Double
    <*> arbitraryReducedMaybe n -- signalEvaluateCoreAttributesP50EodBalance61dTo90d :: Maybe Double
    <*> arbitraryReducedMaybe n -- signalEvaluateCoreAttributesP50EodBalance90d :: Maybe Double
    <*> arbitraryReducedMaybe n -- signalEvaluateCoreAttributesP90EodBalance30d :: Maybe Double
    <*> arbitraryReducedMaybe n -- signalEvaluateCoreAttributesP90EodBalance31dTo60d :: Maybe Double
    <*> arbitraryReducedMaybe n -- signalEvaluateCoreAttributesP90EodBalance60d :: Maybe Double
    <*> arbitraryReducedMaybe n -- signalEvaluateCoreAttributesP90EodBalance61dTo90d :: Maybe Double
    <*> arbitraryReducedMaybe n -- signalEvaluateCoreAttributesP90EodBalance90d :: Maybe Double
    <*> arbitraryReducedMaybe n -- signalEvaluateCoreAttributesP95CreditTransactionsAmount28d :: Maybe Double
    <*> arbitraryReducedMaybe n -- signalEvaluateCoreAttributesP95DebitTransactionsAmount28d :: Maybe Double
    <*> arbitraryReducedMaybe n -- signalEvaluateCoreAttributesPhoneChangeCount28d :: Maybe Int
    <*> arbitraryReducedMaybe n -- signalEvaluateCoreAttributesPhoneChangeCount90d :: Maybe Int
    <*> arbitraryReducedMaybe n -- signalEvaluateCoreAttributesPlaidConnectionsCount30d :: Maybe Int
    <*> arbitraryReducedMaybe n -- signalEvaluateCoreAttributesPlaidConnectionsCount7d :: Maybe Int
    <*> arbitraryReducedMaybe n -- signalEvaluateCoreAttributesPlaidNonOauthAuthenticationAttemptsCount30d :: Maybe Int
    <*> arbitraryReducedMaybe n -- signalEvaluateCoreAttributesPlaidNonOauthAuthenticationAttemptsCount3d :: Maybe Int
    <*> arbitraryReducedMaybe n -- signalEvaluateCoreAttributesPlaidNonOauthAuthenticationAttemptsCount7d :: Maybe Int
    <*> arbitraryReducedMaybe n -- signalEvaluateCoreAttributesTotalCreditTransactionsAmount10d :: Maybe Double
    <*> arbitraryReducedMaybe n -- signalEvaluateCoreAttributesTotalCreditTransactionsAmount30d :: Maybe Double
    <*> arbitraryReducedMaybe n -- signalEvaluateCoreAttributesTotalCreditTransactionsAmount60d :: Maybe Double
    <*> arbitraryReducedMaybe n -- signalEvaluateCoreAttributesTotalCreditTransactionsAmount90d :: Maybe Double
    <*> arbitraryReducedMaybe n -- signalEvaluateCoreAttributesTotalDebitTransactionsAmount10d :: Maybe Double
    <*> arbitraryReducedMaybe n -- signalEvaluateCoreAttributesTotalDebitTransactionsAmount30d :: Maybe Double
    <*> arbitraryReducedMaybe n -- signalEvaluateCoreAttributesTotalDebitTransactionsAmount60d :: Maybe Double
    <*> arbitraryReducedMaybe n -- signalEvaluateCoreAttributesTotalDebitTransactionsAmount90d :: Maybe Double
    <*> arbitraryReducedMaybe n -- signalEvaluateCoreAttributesTotalPlaidConnectionsCount :: Maybe Int
    <*> arbitraryReducedMaybe n -- signalEvaluateCoreAttributesTransactionsLastUpdated :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- signalEvaluateCoreAttributesUnauthorizedTransactionsCount30d :: Maybe Int
    <*> arbitraryReducedMaybe n -- signalEvaluateCoreAttributesUnauthorizedTransactionsCount60d :: Maybe Int
    <*> arbitraryReducedMaybe n -- signalEvaluateCoreAttributesUnauthorizedTransactionsCount7d :: Maybe Int
    <*> arbitraryReducedMaybe n -- signalEvaluateCoreAttributesUnauthorizedTransactionsCount90d :: Maybe Int
  
instance Arbitrary SignalEvaluateRequest where
  arbitrary = sized genSignalEvaluateRequest

genSignalEvaluateRequest :: Int -> Gen SignalEvaluateRequest
genSignalEvaluateRequest n =
  SignalEvaluateRequest
    <$> arbitrary -- signalEvaluateRequestAccessToken :: Text
    <*> arbitrary -- signalEvaluateRequestAccountId :: Text
    <*> arbitrary -- signalEvaluateRequestAmount :: Double
    <*> arbitraryReducedMaybe n -- signalEvaluateRequestClientId :: Maybe Text
    <*> arbitrary -- signalEvaluateRequestClientTransactionId :: Text
    <*> arbitraryReducedMaybe n -- signalEvaluateRequestClientUserId :: Maybe Text
    <*> arbitraryReducedMaybe n -- signalEvaluateRequestDefaultPaymentMethod :: Maybe Text
    <*> arbitraryReducedMaybe n -- signalEvaluateRequestDevice :: Maybe SignalDevice
    <*> arbitraryReducedMaybe n -- signalEvaluateRequestIsRecurring :: Maybe Bool
    <*> arbitraryReducedMaybe n -- signalEvaluateRequestRiskProfileKey :: Maybe Text
    <*> arbitraryReducedMaybe n -- signalEvaluateRequestRulesetKey :: Maybe Text
    <*> arbitraryReducedMaybe n -- signalEvaluateRequestSecret :: Maybe Text
    <*> arbitraryReducedMaybe n -- signalEvaluateRequestUser :: Maybe SignalUser
    <*> arbitraryReducedMaybe n -- signalEvaluateRequestUserPresent :: Maybe Bool
  
instance Arbitrary SignalEvaluateResponse where
  arbitrary = sized genSignalEvaluateResponse

genSignalEvaluateResponse :: Int -> Gen SignalEvaluateResponse
genSignalEvaluateResponse n =
  SignalEvaluateResponse
    <$> arbitraryReducedMaybe n -- signalEvaluateResponseCoreAttributes :: Maybe SignalEvaluateCoreAttributes
    <*> arbitrary -- signalEvaluateResponseRequestId :: Text
    <*> arbitraryReducedMaybe n -- signalEvaluateResponseRiskProfile :: Maybe RiskProfile
    <*> arbitraryReducedMaybe n -- signalEvaluateResponseRuleset :: Maybe Ruleset
    <*> arbitraryReduced n -- signalEvaluateResponseScores :: SignalScores
    <*> arbitraryReduced n -- signalEvaluateResponseWarnings :: [SignalWarning]
  
instance Arbitrary SignalPersonName where
  arbitrary = sized genSignalPersonName

genSignalPersonName :: Int -> Gen SignalPersonName
genSignalPersonName n =
  SignalPersonName
    <$> arbitraryReducedMaybe n -- signalPersonNameFamilyName :: Maybe Text
    <*> arbitraryReducedMaybe n -- signalPersonNameGivenName :: Maybe Text
    <*> arbitraryReducedMaybe n -- signalPersonNameMiddleName :: Maybe Text
    <*> arbitraryReducedMaybe n -- signalPersonNamePrefix :: Maybe Text
    <*> arbitraryReducedMaybe n -- signalPersonNameSuffix :: Maybe Text
  
instance Arbitrary SignalReturnReportRequest where
  arbitrary = sized genSignalReturnReportRequest

genSignalReturnReportRequest :: Int -> Gen SignalReturnReportRequest
genSignalReturnReportRequest n =
  SignalReturnReportRequest
    <$> arbitraryReducedMaybe n -- signalReturnReportRequestClientId :: Maybe Text
    <*> arbitrary -- signalReturnReportRequestClientTransactionId :: Text
    <*> arbitrary -- signalReturnReportRequestReturnCode :: Text
    <*> arbitraryReducedMaybe n -- signalReturnReportRequestReturnedAt :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- signalReturnReportRequestSecret :: Maybe Text
  
instance Arbitrary SignalReturnReportResponse where
  arbitrary = sized genSignalReturnReportResponse

genSignalReturnReportResponse :: Int -> Gen SignalReturnReportResponse
genSignalReturnReportResponse n =
  SignalReturnReportResponse
    <$> arbitrary -- signalReturnReportResponseRequestId :: Text
  
instance Arbitrary SignalScores where
  arbitrary = sized genSignalScores

genSignalScores :: Int -> Gen SignalScores
genSignalScores n =
  SignalScores
    <$> arbitraryReducedMaybe n -- signalScoresBankInitiatedReturnRisk :: Maybe BankInitiatedReturnRisk
    <*> arbitraryReducedMaybe n -- signalScoresCustomerInitiatedReturnRisk :: Maybe CustomerInitiatedReturnRisk
  
instance Arbitrary SignalUser where
  arbitrary = sized genSignalUser

genSignalUser :: Int -> Gen SignalUser
genSignalUser n =
  SignalUser
    <$> arbitraryReducedMaybe n -- signalUserAddress :: Maybe SignalAddressData
    <*> arbitraryReducedMaybe n -- signalUserEmailAddress :: Maybe Text
    <*> arbitraryReducedMaybe n -- signalUserName :: Maybe SignalPersonName
    <*> arbitraryReducedMaybe n -- signalUserPhoneNumber :: Maybe Text
  
instance Arbitrary SignalWarning where
  arbitrary = sized genSignalWarning

genSignalWarning :: Int -> Gen SignalWarning
genSignalWarning n =
  SignalWarning
    <$> arbitraryReducedMaybe n -- signalWarningWarningCode :: Maybe Text
    <*> arbitraryReducedMaybe n -- signalWarningWarningMessage :: Maybe Text
    <*> arbitraryReducedMaybe n -- signalWarningWarningType :: Maybe Text
  
instance Arbitrary TotalInflowAmount where
  arbitrary = sized genTotalInflowAmount

genTotalInflowAmount :: Int -> Gen TotalInflowAmount
genTotalInflowAmount n =
  TotalInflowAmount
    <$> arbitrary -- totalInflowAmountAmount :: Double
    <*> arbitrary -- totalInflowAmountIsoCurrencyCode :: Text
    <*> arbitrary -- totalInflowAmountUnofficialCurrencyCode :: Text
  
instance Arbitrary TotalInflowAmount30d where
  arbitrary = sized genTotalInflowAmount30d

genTotalInflowAmount30d :: Int -> Gen TotalInflowAmount30d
genTotalInflowAmount30d n =
  TotalInflowAmount30d
    <$> arbitrary -- totalInflowAmount30dAmount :: Double
    <*> arbitrary -- totalInflowAmount30dIsoCurrencyCode :: Text
    <*> arbitrary -- totalInflowAmount30dUnofficialCurrencyCode :: Text
  
instance Arbitrary TotalInflowAmount60d where
  arbitrary = sized genTotalInflowAmount60d

genTotalInflowAmount60d :: Int -> Gen TotalInflowAmount60d
genTotalInflowAmount60d n =
  TotalInflowAmount60d
    <$> arbitrary -- totalInflowAmount60dAmount :: Double
    <*> arbitrary -- totalInflowAmount60dIsoCurrencyCode :: Text
    <*> arbitrary -- totalInflowAmount60dUnofficialCurrencyCode :: Text
  
instance Arbitrary TotalInflowAmount90d where
  arbitrary = sized genTotalInflowAmount90d

genTotalInflowAmount90d :: Int -> Gen TotalInflowAmount90d
genTotalInflowAmount90d n =
  TotalInflowAmount90d
    <$> arbitrary -- totalInflowAmount90dAmount :: Double
    <*> arbitrary -- totalInflowAmount90dIsoCurrencyCode :: Text
    <*> arbitrary -- totalInflowAmount90dUnofficialCurrencyCode :: Text
  
instance Arbitrary TotalOutflowAmount where
  arbitrary = sized genTotalOutflowAmount

genTotalOutflowAmount :: Int -> Gen TotalOutflowAmount
genTotalOutflowAmount n =
  TotalOutflowAmount
    <$> arbitrary -- totalOutflowAmountAmount :: Double
    <*> arbitrary -- totalOutflowAmountIsoCurrencyCode :: Text
    <*> arbitrary -- totalOutflowAmountUnofficialCurrencyCode :: Text
  
instance Arbitrary TotalOutflowAmount30d where
  arbitrary = sized genTotalOutflowAmount30d

genTotalOutflowAmount30d :: Int -> Gen TotalOutflowAmount30d
genTotalOutflowAmount30d n =
  TotalOutflowAmount30d
    <$> arbitrary -- totalOutflowAmount30dAmount :: Double
    <*> arbitrary -- totalOutflowAmount30dIsoCurrencyCode :: Text
    <*> arbitrary -- totalOutflowAmount30dUnofficialCurrencyCode :: Text
  
instance Arbitrary TotalOutflowAmount60d where
  arbitrary = sized genTotalOutflowAmount60d

genTotalOutflowAmount60d :: Int -> Gen TotalOutflowAmount60d
genTotalOutflowAmount60d n =
  TotalOutflowAmount60d
    <$> arbitrary -- totalOutflowAmount60dAmount :: Double
    <*> arbitrary -- totalOutflowAmount60dIsoCurrencyCode :: Text
    <*> arbitrary -- totalOutflowAmount60dUnofficialCurrencyCode :: Text
  
instance Arbitrary TotalOutflowAmount90d where
  arbitrary = sized genTotalOutflowAmount90d

genTotalOutflowAmount90d :: Int -> Gen TotalOutflowAmount90d
genTotalOutflowAmount90d n =
  TotalOutflowAmount90d
    <$> arbitrary -- totalOutflowAmount90dAmount :: Double
    <*> arbitrary -- totalOutflowAmount90dIsoCurrencyCode :: Text
    <*> arbitrary -- totalOutflowAmount90dUnofficialCurrencyCode :: Text
  
instance Arbitrary TotalReportInflowAmount where
  arbitrary = sized genTotalReportInflowAmount

genTotalReportInflowAmount :: Int -> Gen TotalReportInflowAmount
genTotalReportInflowAmount n =
  TotalReportInflowAmount
    <$> arbitrary -- totalReportInflowAmountAmount :: Double
    <*> arbitrary -- totalReportInflowAmountIsoCurrencyCode :: Text
    <*> arbitrary -- totalReportInflowAmountUnofficialCurrencyCode :: Text
  
instance Arbitrary TotalReportInflowAmount30d where
  arbitrary = sized genTotalReportInflowAmount30d

genTotalReportInflowAmount30d :: Int -> Gen TotalReportInflowAmount30d
genTotalReportInflowAmount30d n =
  TotalReportInflowAmount30d
    <$> arbitrary -- totalReportInflowAmount30dAmount :: Double
    <*> arbitrary -- totalReportInflowAmount30dIsoCurrencyCode :: Text
    <*> arbitrary -- totalReportInflowAmount30dUnofficialCurrencyCode :: Text
  
instance Arbitrary TotalReportInflowAmount60d where
  arbitrary = sized genTotalReportInflowAmount60d

genTotalReportInflowAmount60d :: Int -> Gen TotalReportInflowAmount60d
genTotalReportInflowAmount60d n =
  TotalReportInflowAmount60d
    <$> arbitrary -- totalReportInflowAmount60dAmount :: Double
    <*> arbitrary -- totalReportInflowAmount60dIsoCurrencyCode :: Text
    <*> arbitrary -- totalReportInflowAmount60dUnofficialCurrencyCode :: Text
  
instance Arbitrary TotalReportInflowAmount90d where
  arbitrary = sized genTotalReportInflowAmount90d

genTotalReportInflowAmount90d :: Int -> Gen TotalReportInflowAmount90d
genTotalReportInflowAmount90d n =
  TotalReportInflowAmount90d
    <$> arbitrary -- totalReportInflowAmount90dAmount :: Double
    <*> arbitrary -- totalReportInflowAmount90dIsoCurrencyCode :: Text
    <*> arbitrary -- totalReportInflowAmount90dUnofficialCurrencyCode :: Text
  
instance Arbitrary TotalReportOutflowAmount where
  arbitrary = sized genTotalReportOutflowAmount

genTotalReportOutflowAmount :: Int -> Gen TotalReportOutflowAmount
genTotalReportOutflowAmount n =
  TotalReportOutflowAmount
    <$> arbitrary -- totalReportOutflowAmountAmount :: Double
    <*> arbitrary -- totalReportOutflowAmountIsoCurrencyCode :: Text
    <*> arbitrary -- totalReportOutflowAmountUnofficialCurrencyCode :: Text
  
instance Arbitrary TotalReportOutflowAmount30d where
  arbitrary = sized genTotalReportOutflowAmount30d

genTotalReportOutflowAmount30d :: Int -> Gen TotalReportOutflowAmount30d
genTotalReportOutflowAmount30d n =
  TotalReportOutflowAmount30d
    <$> arbitrary -- totalReportOutflowAmount30dAmount :: Double
    <*> arbitrary -- totalReportOutflowAmount30dIsoCurrencyCode :: Text
    <*> arbitrary -- totalReportOutflowAmount30dUnofficialCurrencyCode :: Text
  
instance Arbitrary TotalReportOutflowAmount60d where
  arbitrary = sized genTotalReportOutflowAmount60d

genTotalReportOutflowAmount60d :: Int -> Gen TotalReportOutflowAmount60d
genTotalReportOutflowAmount60d n =
  TotalReportOutflowAmount60d
    <$> arbitrary -- totalReportOutflowAmount60dAmount :: Double
    <*> arbitrary -- totalReportOutflowAmount60dIsoCurrencyCode :: Text
    <*> arbitrary -- totalReportOutflowAmount60dUnofficialCurrencyCode :: Text
  
instance Arbitrary TotalReportOutflowAmount90d where
  arbitrary = sized genTotalReportOutflowAmount90d

genTotalReportOutflowAmount90d :: Int -> Gen TotalReportOutflowAmount90d
genTotalReportOutflowAmount90d n =
  TotalReportOutflowAmount90d
    <$> arbitrary -- totalReportOutflowAmount90dAmount :: Double
    <*> arbitrary -- totalReportOutflowAmount90dIsoCurrencyCode :: Text
    <*> arbitrary -- totalReportOutflowAmount90dUnofficialCurrencyCode :: Text
  
instance Arbitrary UserCreateRequest where
  arbitrary = sized genUserCreateRequest

genUserCreateRequest :: Int -> Gen UserCreateRequest
genUserCreateRequest n =
  UserCreateRequest
    <$> arbitraryReducedMaybe n -- userCreateRequestClientId :: Maybe Text
    <*> arbitrary -- userCreateRequestClientUserId :: Text
    <*> arbitraryReducedMaybe n -- userCreateRequestConsumerReportUserIdentity :: Maybe ConsumerReportUserIdentity
    <*> arbitraryReducedMaybe n -- userCreateRequestEndCustomer :: Maybe Text
    <*> arbitraryReducedMaybe n -- userCreateRequestIdentity :: Maybe ClientUserIdentity
    <*> arbitraryReducedMaybe n -- userCreateRequestSecret :: Maybe Text
    <*> arbitraryReducedMaybe n -- userCreateRequestWithUpgradedUser :: Maybe Bool
  
instance Arbitrary UserCreateResponse where
  arbitrary = sized genUserCreateResponse

genUserCreateResponse :: Int -> Gen UserCreateResponse
genUserCreateResponse n =
  UserCreateResponse
    <$> arbitrary -- userCreateResponseRequestId :: Text
    <*> arbitrary -- userCreateResponseUserId :: Text
    <*> arbitraryReducedMaybe n -- userCreateResponseUserToken :: Maybe Text
  
instance Arbitrary UserIDNumber where
  arbitrary = sized genUserIDNumber

genUserIDNumber :: Int -> Gen UserIDNumber
genUserIDNumber n =
  UserIDNumber
    <$> arbitraryReduced n -- userIDNumberType :: IDNumberType
    <*> arbitrary -- userIDNumberValue :: Text
  
instance Arbitrary BaseReportTransactionType where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary BaseReportWarningCode where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary CashflowAttributesVersion where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary CheckReportWarningCode where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary ConsumerDisputeCategory where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary ConsumerReportPermissiblePurpose where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary CraBankIncomeBonusType where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary CraBankIncomeStatus where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary CraBankIncomeWarningCode where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary CraCheckReportPermissiblePurpose where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary CraCheckReportVerificationGetReportType where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary CraPDFAddOns where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary CraPartnerInsightsBaseFicoScoreVersion where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary CraPartnerInsightsBureau where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary CraPartnerInsightsUltraFicoScoreVersion where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary CraUserTier where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary CreditBankIncomeAccountType where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary CreditBankIncomeCategory where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary CreditBankIncomeErrorType where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary CreditBankIncomePayFrequency where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary CreditBankIncomeWarningType where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary DepositoryAccountSubtype where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary GSEReportType where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary IDNumberType where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary IncomeInsightsVersion where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary NetworkInsightsVersion where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary OwnershipType where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary PersonalFinanceCategoryVersion where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary PlaidErrorType where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary PlaidLendScoreVersion where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary PrismCashScoreVersion where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary PrismDetectVersion where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary PrismExtendVersion where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary PrismFirstDetectVersion where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary PrismInsightsVersion where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary RuleResult where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary SignalDecisionOutcome where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary SignalPaymentMethod where
  arbitrary = arbitraryBoundedEnum

