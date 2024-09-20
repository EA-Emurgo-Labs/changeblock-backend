module EA.CLI.Parser (
  parseRootCommand,
  pref,
) where

import EA.CLI.Auth.Parser (parseAuthCommand)
import EA.CLI.Marketplace.Parser (parseMarketplaceCommand)
import EA.CLI.Oracle.Parser (parseOracleCommand)
import EA.CLI.Wallet.Parser (parseWalletCommand)

import EA.CLI.Run (RootCommand (..))

import Options.Applicative (
  CommandFields,
  Mod,
  Parser,
  ParserInfo,
  ParserPrefs,
  command,
  commandGroup,
  fullDesc,
  header,
  helper,
  info,
  metavar,
  prefs,
  progDesc,
  showHelpOnEmpty,
  subparser,
 )

parseRootCommand :: ParserInfo RootCommand
parseRootCommand =
  info (pRootCommand <**> helper) $
    mconcat
      [ fullDesc
      , header $
          mconcat
            [ "Nexchange-cli - Command line utility for interacting with the Nexchange API and blockchain."
            , " Provides specific commands to manage Authentication keys, Marketplace, Oracle and Wallet."
            ]
      , progDesc "Note: To submit blockchain transactions, you need to have CORE_CONFIG_PATH variable set. (default: config.json)"
      ]

pRootCommand :: Parser RootCommand
pRootCommand =
  asum
    [ pAuthCommand
    , pMarketplaceCommand
    , pOracleCommand
    , pWalletCommand
    ]

pAuthCommand :: Parser RootCommand
pAuthCommand =
  fmap AuthCommand $
    subparser $
      mconcat
        [ commandGroup "Auth Related Commands"
        , metavar "Auth Related Commands"
        , command' "auth" "Auth related commands" parseAuthCommand
        ]

pMarketplaceCommand :: Parser RootCommand
pMarketplaceCommand =
  fmap MarketplaceCommand $
    subparser $
      mconcat
        [ commandGroup "Marketplace Related Commands"
        , metavar "Marketplace Related Commands"
        , command' "marketplace" "Marketplace related commands" parseMarketplaceCommand
        ]

pOracleCommand :: Parser RootCommand
pOracleCommand =
  fmap OracleCommand $
    subparser $
      mconcat
        [ commandGroup "Oracle Related Commands"
        , metavar "Oracle Related Commands"
        , command' "oracle" "Oracle related commands" parseOracleCommand
        ]

pWalletCommand :: Parser RootCommand
pWalletCommand =
  fmap WalletCommand $
    subparser $
      mconcat
        [ commandGroup "Wallet Related Commands"
        , metavar "Wallet Related Commands"
        , command' "wallet" "Wallet related commands" parseWalletCommand
        ]

command' :: String -> String -> Parser a -> Mod CommandFields a
command' c descr p =
  mconcat
    [ command c (info (p <**> helper) $ mconcat [progDesc descr])
    , metavar c
    ]

pref :: ParserPrefs
pref =
  prefs $
    mconcat
      [ showHelpOnEmpty
      ]