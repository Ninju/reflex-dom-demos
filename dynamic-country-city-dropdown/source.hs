import Reflex
import Reflex.Dom
import qualified Data.Map as Map
import Control.Monad

clone x = (x,x)
mapFromClones = Map.fromList . map clone

countries = mapFromClones $ map fst countriesMap
countriesMap =
  [ ("United States", ["Chicago", "New York", "New Jersey", "Los Angeles", "Texas", "Colorado"])
  , ("United Kingdom", ["London", "York", "Birmingham", "Manchester", "Cardiff", "Sheffield", "Liverpool", "Glasgow"])
  , ("Brazil", ["Rio de Janeiro", "Salvador", "Belo Horizonte", "Recife", "Manaus", "Porto Alegre", "Mato Grosso"])
  ]

findCities country = let maybeCities = lookup country countriesMap in concat maybeCities

citiesOfSelectedCountry countryName = mapDyn (mapFromClones . findCities) countryName

main = mainWidget $ do
  el "h1" $ text "Dynamic country-city dropdown"

  countryDropdown <- el "div" $ do
    elAttr "label" ("for" =: "country-dropdown") $ text "Select a country:"
    dropdown "" (constDyn countries) $ def & attributes .~ constDyn (Map.fromList [("name", "country-dropdown"), ("id", "country-dropdown")])

  el "div" $ do
    let selectedCountryName = _dropdown_value countryDropdown
    elAttr "label" ("for" =: "city-dropdown") $ text "Select a city:"
    cities <- citiesOfSelectedCountry selectedCountryName
    void $ dropdown "" cities $ def & attributes .~ constDyn (Map.fromList [("name", "city-dropdown"), ("id", "city-dropdown")])
