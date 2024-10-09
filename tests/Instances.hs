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

