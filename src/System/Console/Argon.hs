-- extensions {{{
{-# LANGUAGE GADTs #-}
-- }}}

-- exports {{{
module System.Console.Argon
    ( getCmdLine
    , parseCmdLine
    , helpCmdLine
    , CmdLineError(..)

    , CmdLine()
    , flag
    , argument, optArgument
    , arguments, optArguments
    , subcommands, optSubcommands
    , docPara

    , strArg
    , intArg
    ) where
-- }}}

-- imports {{{
import           Control.Applicative
import           Control.Monad.Except
import           Control.Monad.Reader
import           Data.List (intercalate)
import qualified Data.Map as M
import           Data.Map (Map)
import           Text.Read (readEither)
import           System.Environment (getArgs, getProgName)
import           System.Exit (exitFailure)
-- }}}

-- front-end {{{

getCmdLine :: CmdLine a -> IO a
getCmdLine cl = do
    progName <- getProgName
    args <- getArgs
    case parseCmdLine [progName] cl args of
        Right x -> return x
        Left err -> do
            putStr $ case err of
                CmdLineError msg -> msg
                CmdLineHelp msg -> msg
            exitFailure

-- }}}

-- cmdline {{{

data CmdLine a where

    -- applicative glue
    CLpure :: a -> CmdLine a
    CLapply :: CmdLine (a -> b) -> CmdLine a -> CmdLine b

    CLflag :: FlagSpec -> CmdLine Bool
    CLarg :: ArgSpec a -> CmdLine a
    CLargs :: ArgSpec a -> CmdLine [a]
    CLsubs :: SubSpec a -> CmdLine a

    CLdocp :: String -> CmdLine ()

type OptNames = ([Char], [String])
type ArgParse a = String -> Either String a

data FlagSpec = FlagSpec
    { flagNames :: OptNames
    , flagDefault :: Bool
    , flagDoc :: String
    }

data ArgSpec a
    = OptionalSpec
        { argOptNames :: OptNames
        , argMetavar :: String
        , argDefault :: a
        , argParse :: ArgParse a
        , argDoc :: String
        }
    | MandatorySpec
        { argMetavar :: String
        , argParse :: ArgParse a
        , argDoc :: String
        }

data SubSpec a = SubSpec
    { subChoices :: [[String]]
    , subCommands :: Map String (CmdLine a)
    , subDefault :: Maybe a
    }

instance Functor CmdLine where
    fmap = CLapply . CLpure

instance Applicative CmdLine where
    pure = CLpure
    (<*>) = CLapply

-- constructors {{{

flag :: String -> Bool -> String -> CmdLine Bool
flag names def doc = CLflag $ FlagSpec (optNames names) def doc

argument :: String -> ArgParse a -> String -> CmdLine a
argument var parse doc = CLarg $ MandatorySpec var parse doc

arguments :: String -> ArgParse a -> String -> CmdLine [a]
arguments var parse doc = CLargs $ MandatorySpec var parse doc

optArgument :: String -> String -> a -> ArgParse a -> String -> CmdLine a
optArgument names var def parse doc = CLarg $ OptionalSpec (optNames names) var def parse doc

optArguments :: String -> String -> ArgParse a -> String -> CmdLine [a]
optArguments names var parse doc = CLargs $ OptionalSpec (optNames names) var undefined parse doc

subcommands :: [(String, CmdLine a)] -> CmdLine a
subcommands = CLsubs . makeSubs Nothing

optSubcommands :: a -> [(String, CmdLine a)] -> CmdLine a
optSubcommands def = CLsubs . makeSubs (Just def)

docPara :: String -> CmdLine ()
docPara = CLdocp

-- }}}

-- argument parsers {{{

strArg :: ArgParse String
strArg = Right

intArg :: ArgParse Int
intArg = readEither

-- }}}

-- utilites {{{

optNames :: String -> OptNames
optNames s = (short, long)
  where
    toks = tokenize $ words s
    short = [x | TokShort x _ <- toks]
    long = [x | TokLong x _ <- toks]

optShow :: OptNames -> String
optShow (ss, ls) = intercalate "|" $
    map (('-':).(:[])) ss
    ++ map (('-':).('-':)) ls

makeSubs :: Maybe a -> [(String, CmdLine a)] -> SubSpec a
makeSubs def choices = SubSpec
    { subChoices = [words s | (s, _) <- choices]
    , subCommands = M.unions [M.fromList [(c, x) | c <- words s] | (s, x) <- choices]
    , subDefault = def
    }

-- }}}

-- }}}

-- sketching {{{

data Sketch = Sketch
    { skShortOpts :: Map Char Bool -- true if an option takes an argument
    , skLongOpts :: Map String Bool
    , skMandArgs :: Int
    , skOptArgs :: Int
    , skArgList :: Bool
    , skSubcmd :: Bool
    , skDocParas :: [String]
    }

emptySketch = Sketch
    { skShortOpts = M.empty
    , skLongOpts = M.singleton "help" False
    , skMandArgs = 0
    , skOptArgs = 0
    , skArgList = False
    , skSubcmd = False
    , skDocParas = []
    }

sketch :: CmdLine a -> Sketch
sketch cl = go cl done emptySketch
  where
    done sk = sk { skDocParas = reverse $ skDocParas sk }

    go :: CmdLine a -> (Sketch -> Sketch) -> Sketch -> Sketch
    go cl k sk = ($ sk) $ case cl of
        CLpure _ -> k
        CLapply f x -> go f $ go x k
        CLflag spec -> flag spec k
        CLarg spec -> arg spec k
        CLargs spec -> args spec k
        CLsubs spec -> subs spec k
        CLdocp s -> docp s k

    flag spec k sk = k $ opts (flagNames spec) False sk

    arg spec k sk = k $ case spec of
        OptionalSpec {argOptNames = x}
            | x == ([], []) -> bare False sk
            | otherwise -> opts x True sk
        _ -> bare True sk

    args spec k sk = k $ case spec of
        OptionalSpec {argOptNames = x}
            | x == ([], []) -> arglist sk
            | otherwise -> opts x True sk
        _ -> arglist sk

    subs spec k sk
        | skSubcmd sk = error "CmdLine: multiple subcommands cannot be combined."
        | otherwise = sk { skSubcmd = True }

    docp s k sk = k $ sk { skDocParas = s : skDocParas sk }

    arglist sk
        | skArgList sk = error "CmdLine: only one argument list can be used."
        | skSubcmd sk = error "CmdLine: argument lists cannot be combined with subcommands."
        | otherwise = sk { skArgList = True }

    bare True sk@(Sketch {skMandArgs = n})
        | skOptArgs sk > 0 = error "CmdLine: all mandatory arguments must come before any optional ones."
        | skArgList sk = error "CmdLine: further arguments cannot appear after an argument list."
        | skSubcmd sk = error "CmdLine: further arguments cannot appear after a subcommand."
        | otherwise = sk { skMandArgs = succ n }
    bare False sk@(Sketch {skOptArgs = n})
        | skArgList sk = error "CmdLine: further arguments cannot appear after an argument list."
        | skSubcmd sk = error "CmdLine: further arguments cannot appear after a subcommand."
        | otherwise = sk { skOptArgs = succ n }

    opts (ss, ls) v sk@(Sketch {skShortOpts = sopts, skLongOpts = lopts}) = let
        { ss' = M.fromList [(s, v) | s <- ss]
        ; ls' = M.fromList [(l, v) | l <- ls]
        ; sOverlap = M.intersection sopts ss'
        ; lOverlap = M.intersection lopts ls'
        ; result
            | not $ M.null sOverlap && M.null lOverlap = error . concat
                $ "CmdLine: conflicting options:"
                : map ((' ':).('-':).(:[])) (M.keys sOverlap)
                ++ map ((' ':).('-':).('-':)) (M.keys lOverlap)
            | otherwise = sk
                { skShortOpts = M.union sopts ss'
                , skLongOpts = M.union lopts ls'
                }
        } in result

-- }}}

-- tokens {{{

data Token
    = TokShort Char (Maybe String)
    | TokLong String (Maybe String)
    | TokBare String
  deriving (Show, Eq)

tokenize :: [String] -> [Token]
tokenize = go
  where
    go [] = []
    go (s:ss) = case s of
        "--" -> map TokBare ss
        '-':'-':s' -> case break (== '=') s' of
            (opt, ('=':arg)) -> TokLong opt (Just arg) : go ss
            (opt, _) -> TokLong opt Nothing : go ss
        '-':opt:s' -> case s' of
            [] -> TokShort opt Nothing : go ss
            _ -> TokShort opt (Just s') : go ss
        _ -> TokBare s : go ss

-- }}}

-- parsing {{{

data CmdLineError
    = CmdLineError String
    | CmdLineHelp String
  deriving (Show)

-- infrastructure {{{

type Parser = ExceptT CmdLineError (Reader ParserCtx)

data ParserCtx = ParserCtx
    { parserPrefix :: [String] -- reverse order
    , parserHelp :: String
    , parserWhile :: [String]
    }

abort :: String -> Parser a
abort reason = do
    whiles <- asks parserWhile
    let text = unlines $ concat
            [ [ "Error: " ++ reason ]
            , map (\l -> concat ["    while ", l, ";"]) whiles
            , [ "    while parsing command-line arguments."
              , "Use `--help` for usage information."
              ]
            ]
    throwError $ CmdLineError text

whileDoing :: String -> Parser a -> Parser a
whileDoing thing = local $ \ctx ->
    ctx { parserWhile = thing : parserWhile ctx }

enterSub :: String -> CmdLine a -> Parser b -> Parser b
enterSub name cl = local $ \ctx -> ParserCtx
    { parserPrefix = name : parserPrefix ctx
    , parserHelp = helpCmdLine (reverse $ name : parserPrefix ctx) cl
    , parserWhile = concat ["parsing the subcommand '", name, "'"]
                  : parserWhile ctx
    }

-- }}}

parseCmdLine :: [String] -> CmdLine a -> [String] -> Either CmdLineError a
parseCmdLine pfx root ss = runReader
    (runExceptT (parseInternal root toks))
    $ ParserCtx
    { parserPrefix = reverse pfx
    , parserHelp = helpCmdLine pfx root
    , parserWhile = []
    }
  where
    toks = tokenize ss

-- viscera {{{

parseInternal :: CmdLine a -> [Token] -> Parser a
parseInternal = cmd
  where
    cmd :: CmdLine a -> [Token] -> Parser a
    cmd root ts = do
        let sk = sketch root
        (opts, args, ts') <- case chompAll sk ts of
            Right (opts, args, ts') -> return (opts, args, ts')
            Left reason -> abort reason
        when (not . null $ occurences ("", ["help"]) opts) $ do
            msg <- asks parserHelp
            throwError $ CmdLineHelp msg
        (x, _) <- go sk root opts args ts'
        return x

    go :: Sketch -> CmdLine a
       -> [Token] -> [String] -> [Token]
       -> Parser (a, [String])
    go sk cl opts args rest = case cl of
        CLpure x -> return (x, args)
        CLapply f x -> do
            (f', args') <- go sk f opts args rest
            (x', args'') <- go sk x opts args' rest
            return (f' x', args'')

        CLflag spec -> do
            let present = not . null $ occurences (flagNames spec) opts
            let def = flagDefault spec
            return (if present then not def else def, args)

        CLarg spec -> case spec of
            OptionalSpec {}
                | argOptNames spec == ([], []) -> bare spec args
                | otherwise -> do
                    x <- option spec opts
                    return (x, args)
            _ -> bare spec args

        CLargs spec -> case spec of
            OptionalSpec {}
                | argOptNames spec == ([], []) -> arglist spec args
                | otherwise -> do
                    xs <- options spec opts
                    return (xs, args)
            _ -> arglist spec args

        CLsubs spec -> case rest of
            TokBare s : rest' -> case M.lookup s $ subCommands spec of
                Just x -> do
                    res <- enterSub s x $ cmd x rest'
                    return (res, [])
                Nothing -> abort $ concat ["unrecognized subcommand: ", s]
            [] -> case subDefault spec of
                Just def -> return (def, [])
                _ -> abort "subcommand missing"

    argp :: ArgSpec a -> String -> Parser a
    argp spec arg = whileDoing ("parsing the argument '" ++ arg ++ "'")
        $ case argParse spec arg of
            Right x -> return x
            Left err -> abort err

    bare :: ArgSpec a -> [String] -> Parser (a, [String])
    bare spec args = case args of
        arg : args' -> do
            x <- argp spec arg
            return (x, args')
        _ -> case spec of
            OptionalSpec {argDefault = def} -> return (def, args)

    arglist :: ArgSpec a -> [String] -> Parser ([a], [String])
    arglist spec args = do
        xs <- mapM (argp spec) args
        return (xs, [])

    option :: ArgSpec a -> [Token] -> Parser a
    option spec opts = case occurences (argOptNames spec) opts of
        [] -> return $ argDefault spec
        [Just arg] -> argp spec arg
        _ -> abort "multiple occurences of options"

    options :: ArgSpec a -> [Token] -> Parser [a]
    options spec opts = do
        let xs = [x | Just x <- occurences (argOptNames spec) opts]
        mapM (argp spec) xs

    occurences :: OptNames -> [Token] -> [Maybe String]
    occurences (ss, ls) ts = do
        tok <- ts
        case tok of
            TokShort opt x
                | opt `elem` ss -> [x]
            TokLong opt x
                | opt `elem` ls -> [x]
            _ -> []

-- }}}

-- chomping {{{

chompOpt :: Sketch -> [Token] -> Either String (Token, [Token])
chompOpt sk [] = Left "option expected"
chompOpt sk (t:ts) = case t of
    TokShort c marg -> go c marg ('-':[c]) TokShort (skShortOpts sk) ts
    TokLong s marg -> go s marg ('-':'-':s) TokLong (skLongOpts sk) ts
    TokBare _ -> Left $ "option expected"
  where
    go opt marg name ctor omap ts = case marg of
        Nothing -> case M.lookup opt omap of
            Just False -> Right (ctor opt Nothing, ts)
            Just True -> case ts of
                TokBare arg : ts' -> Right (ctor opt $ Just arg, ts')
                _ -> Left $ concat ["option '", name, "' requires an argument, but none given"]
            Nothing -> Left $ concat ["unrecognized option '", name, "'."]
        Just arg -> case M.lookup opt omap of
            Just True -> Right (ctor opt marg, ts)
            Just False -> Left $ concat ["option '", name, "' given an unexpected argument"]
            Nothing -> Left $ concat ["unrecognized option '", name, "'"]

chompAll :: Sketch -> [Token] -> Either String ([Token], [String], [Token])
chompAll sk = go 0 0 [] []
  where
    go mas oas os bs []
        | mas < skMandArgs sk = Left "mandatory arguments missing"
        | otherwise = Right (reverse os, reverse bs, [])
    go mas oas os bs tz@(t:ts) = case t of
        TokBare arg
            | mas < skMandArgs sk -> go (succ mas) oas os (arg:bs) ts
            | oas < skOptArgs sk -> go mas (succ oas) os (arg:bs) ts
            | skArgList sk -> go mas oas os (arg:bs) ts
            | skSubcmd sk -> Right (reverse os, reverse bs, tz)
            | otherwise -> Left "too many arguments given"
        _ -> do
            (o, ts') <- chompOpt sk tz
            case o of
                TokLong "help" _ -> Right ([o], [], [])
                _ -> go mas oas (o:os) bs ts'

-- }}}

-- }}}

-- help messages {{{

-- formatting {{{

formatPara' :: Int -> Int -> String -> [String]
formatPara' whead wtail = go whead (-1) [] . words
  where
    go _ _ [] [] = []
    go _ _ ln [] = [unwords $ reverse ln]
    go width k ln wz@(w:ws) = let
        { l = length w
        ; k' = k + l + 1
        ; res
            | l > width && k + 1 < width
                = case splitAt (width - k - 2) w of
                    (w1, w2) -> (unwords . reverse $ (w1++"-") : ln)
                                : newline (w2:ws)
            | k' > width = (unwords $ reverse ln) : newline wz
            | otherwise = go width k' (w:ln) ws
        } in res
    newline = go wtail (-1) []

formatPara :: Int -> String -> [String]
formatPara width = formatPara' width width

formatParaHang :: Int -> Int -> String -> [String]
formatParaHang width ind s = case formatPara' width (width - ind) s of
    [] -> []
    first:rest -> first : indentPara ind rest

indentPara :: Int -> [String] -> [String]
indentPara k = map (replicate k ' ' ++)

-- }}}

helpCmdLine :: [String] -> CmdLine a -> String
helpCmdLine pfx root = unlines . concat $
    [ case paras of
        [] -> []
        p:_ -> p ++ [""]
    , ["Usage:"]
    , indentPara 4 . formatParaHang (width - 4) 8 $ helpUsage
    , helpArgs
    , helpOpts
    , helpSubs
    ] ++ map ("" :) (drop 1 paras)
  where
    width = 80
    sk = sketch root
    paras = map (formatPara width <=< lines) $ skDocParas sk

    helpUsage = unwords . (pfx ++) . ("[OPTIONS]" :) $ map fst rootArgs

    helpArgs
        | null rootArgs = []
        | otherwise = ("The arguments are:" :) $ do
            (s, txt) <- rootArgs
            indentPara 4 . (s :) . indentPara 4 $
                formatPara (width - 8) txt
                --concat [s, " -- ", txt]

    helpOpts
        | null rootOpts = []
        | otherwise = ("The options are:" :) $ do
            (s, txt) <- rootOpts
            indentPara 4 . (s :) . indentPara 4 $
                formatPara (width - 8) txt

    helpSubs
        | null rootSubs = []
        | otherwise = ("The subcommands are:" :) $ do
            s <- rootSubs
            indentPara 4 [s]

    rootArgs = args root
    args :: CmdLine a -> [(String, String)]
    args cl = case cl of
        CLapply f x -> args f ++ args x

        CLarg spec -> go False spec
        CLargs spec -> go True spec
        CLsubs spec -> [(wrap True False "COMMAND", "Subcommands.")]
        _ -> []
      where
        go dots spec = case spec of
            OptionalSpec {argOptNames = x}
                | x == ([], []) -> [(wrap dots True $ argMetavar spec, argDoc spec)]
                | otherwise -> []
            _ -> [(wrap dots False $ argMetavar spec, argDoc spec)]

        wrap dots br
            = (if br then ('[':).(++"]") else id)
            . (if dots then (++"...") else id)

    rootOpts = opts root
    opts :: CmdLine a -> [(String, String)]
    opts cl = case cl of
        CLapply f x -> opts f ++ opts x

        CLflag spec -> [(optShow $ flagNames spec, flagDoc spec)]
        CLarg spec -> go spec
        CLargs spec -> go spec
        _ -> []
      where
        go spec = case spec of
            OptionalSpec {argOptNames = x, argMetavar = v}
                | x == ([], []) -> []
                | otherwise -> [(optShow x ++ ' ' : v, argDoc spec)]
            _ -> []

    rootSubs = subs root
    subs :: CmdLine a -> [String]
    subs cl = case cl of
        CLapply f x -> subs f ++ subs x

        CLsubs spec -> map (intercalate " | ") $ subChoices spec
        _ -> []

-- }}}

-- vim:fdm=marker:
