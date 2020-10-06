module Client.Parse where

import Client.Types
import Text.Parsec

pathParse :: Parsec String () String
pathParse = many1 (alphaNum <|> oneOf "/.@-_+")

parseCommand :: Maybe String -> Either ParseError InputCommand
parseCommand Nothing = Right ExitCmd
parseCommand (Just "") = Right SkipCmd
parseCommand (Just x) = parse commands "" x

commands :: Parsec String () InputCommand
commands =
  choice
    [ try
        ( string "q" <|> string "quit"
            <|> string "exit"
            <|> string "bye"
        )
        >> return ExitCmd,
      try $ string "init" >> return InitCmd,
      try $ string "pwd" >> return PWDCmd,
      try $ string "help" >> return HelpCmd,
      try $ string "echo" >> spaces >> EchoCmd <$> many anyChar,
      try $ string "cowsay" >> spaces >> CowSayCmd <$> many anyChar,
      try $ string "myip" >> return MyIpCmd,
      try $ string "touch" >> spaces >> TouchCmd <$> pathParse,
      try $ string "get" >> spaces >> GetCmd <$> pathParse,
      try $ string "put" >> spaces >> PutCmd <$> pathParse,
      try $ string "rm" >> spaces >> RemoveCmd <$> pathParse,
      try $ string "fileinfo" >> spaces >> FileInfoCmd <$> pathParse,
      try $ string "cp" >> spaces >> CopyCmd <$> pathParse <*> (spaces >> pathParse),
      try $ string "mv" >> spaces >> MoveCmd <$> pathParse <*> (spaces >> pathParse),
      try $ string "mkdir" >> spaces >> MakeDirCmd <$> pathParse,
      try $ string "rmdir" >> spaces >> RemoveDirCmd <$> pathParse,
      try $ string "dirinfo" >> spaces >> DirInfoCmd <$> pathParse
    ]

helpList :: String
helpList =
  (++) "List of available commands:\n" $
    mconcat . map (\x -> "  " ++ x ++ "\n") $
      [ "q",
        "quit",
        "exit",
        "bye",
        "init",
        "help",
        "pwd",
        "echo",
        "cowsay",
        "myip",
        "touch",
        "get",
        "put",
        "rm",
        "fileinfo",
        "cp",
        "mv",
        "mkdir",
        "rmdir",
        "dirinfo"
      ]
