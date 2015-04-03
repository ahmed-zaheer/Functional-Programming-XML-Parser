
{- ------------- BEGIN: Task 1 -------------------}

-- It represents XML Document
import Control.Monad
import Data.Char
import System.IO
import System.Environment

type XMLtree = Document

-- The following block define types As given in Problem specification
type Document = (Prolog,Element)

type Prolog = XMLDecl

type XMLDecl = (VersionInfo,EncodingDecl,SDDecl)

type VersionInfo = [Char]

type EncodingDecl = [Char]

data SDDecl = Yes | No
	deriving (Show)

	
data Element = XEET EmptyElement| XNEET NotEmptyElement
		deriving(Show)

type EmptyElement = (Name,[Attribute])	

type STag = (Name,[Attribute])

type ETag = Name

type Attribute = (Name,Attvalue)

type Attvalue = XMLString

type Contents = [Content]	

type CharData = [Char]

type XMLString = [Char]

type NotEmptyElement = (STag,Contents,ETag)

data Content = CELE Element | CCD CharData | Empty
		deriving(Show)

type Name = [Char]		

{- ------------- END: Task 1 -------------------}


{- ------------- BEGIN: Task 2 -----------------}


{-
main = do
	contents <- readFile "document.xml"
	putStr contents
-}
parseXML::String->XMLtree
parseXML s = parseDocument s

parseDocument s = ( (fst $ parseProlog s), (parseElement $ snd $ parseProlog s) )

--Parse Prolog
parseProlog ('<':'?':xs) = (parseXMLDecl xs,(removeTillXDecl xs))
parseProlog (x:xs) = if isWhiteSpace x then parseProlog xs else error "Invalid Prolog"

isWhiteSpace s = (s	==' '|| s=='\n'||s=='\r'||s=='\t')

removeTillXDecl ('?':'>':xs) = xs
removeTillXDecl (x:xs) = removeTillXDecl xs

parseXMLDecl s = ((parseVerInfo s,parseEncoding s,parseSDDecl s))

parseVerInfo s = if take 7 s == "version" then takeTillQuote(drop 9 s) else parseVerInfo (tail s)
parseEncoding s = if take 8 s == "encoding" then takeTillQuote(drop 10 s) else parseEncoding (tail s)
parseSDDecl s = if take 10 s == "standalone" then if (takeTillQuote(drop 12 s)) == "yes" then Yes else No 
					else parseSDDecl (tail s)

takeTillQuote s = takeWhile (/='"') s

parseElement s
			| head s == '<' =	if isEmptyElement (tail s) then									
									XEET (fst $ parseEmptyElement s) 
								else
									XNEET (fst $ parseNotEmptyElement s)
			| otherwise = parseElement (tail s)		

isEmptyElement s = if head s == '/' then if head(tail s)== '>' then True else isEmptyElement(tail s)
					else if head s == '>' then False 
					else isEmptyElement (tail s)

parseEmptyElement ('<':xs) = ((fst(parseElemName xs),fst(parseAttribute (snd(parseElemName xs)))),removeTillClosedTag xs)

removeTillClosedTag s = if head s == '>' then if length s > 1 then tail s else []
							else if(take 2 s) == "/>" then if length s > 2 then drop 2 s else []
							else removeTillClosedTag (tail s)	

parseNotEmptyElement s = (( (fst $ parseStag s),
				 (parseContents $ takeStringContent(fst $ fst $ parseStag s , snd $ parseStag s) ),
				 (fst $ parseEtag ( (fst $ fst $ parseStag s , snd (parseStag s)) ))
				),
				snd $ parseEtag ( (fst $ fst $ parseStag s , snd (parseStag s)) ))
				
takeStringContent s =	if (take ((length $ fst s)+3) (snd s) ) == '<':'/':(fst s)++">" then
							[]
						else
							(head $ snd s):takeStringContent (fst s,tail $ snd s)
							
parseContents [] = []
parseContents s = (fst $ parseContent(s)):parseContents(snd $ parseContent(s))

parseContent s
					| s == [] = (Empty,[])
					| isWhiteSpace (head s) = parseContent (tail s)
					| head s == '<' = ( CELE (fst $ parseContEle s) , snd $ parseContEle s )
					| otherwise = ( CCD ( parseChar $ takeTillStartTag s) , [] )

takeTillStartTag s = takeWhile (/='<') s		

parseContEle s = if isEmptyElement (tail s) then
					( XEET (fst $ parseEmptyElement s) , (snd $ parseEmptyElement s) )
				else
					( XNEET (fst $ parseNotEmptyElement s) , (snd $ parseNotEmptyElement s) )

parseChar s = s

parseStag s
			| isWhiteSpace (head s) = parseStag (tail s)
			| head s == '<' = ( (fst $ parseElemName $ tail s,fst $ parseAttribute $ snd $ parseElemName $ tail s),removeTillClosedTag $ tail s )

parseElemName s				
				| isWhiteSpace (head s) = parseElemName (tail s)
				| otherwise = (takeTillClosedTagOrSpace s,(removeTillClosedTagOrSpace s))


takeTillClosedTagOrSpace s = if length (takeTillClosedTag s) > length (takeTillSpace s) && length (takeTillSpace s) > 0 then
								takeTillSpace s
							else
								takeTillClosedTag s								
takeTillClosedTag [] = []
takeTillClosedTag (x:xs) = if x == '>' || (x == '/' && head xs == '>') then
					[]
				else
					x:takeTillClosedTag(xs)					
takeTillSpace s = takeWhile (isNotWhiteSpace) s				
				
				
removeTillClosedTagOrSpace s  =	if length (removeTillClosedTag s) > length (removeTillSpace s) then
									removeTillClosedTag s
									else
										removeTillSpace s

removeTillSpace s = dropWhile isNotWhiteSpace s
isNotWhiteSpace s = not(s==' '||s=='\n'||s=='\r'||s=='\t')		
										
parseAttributeName [] = []
parseAttributeName s = 	if  isWhiteSpace (head s) then
						if length s > 1 then
							parseAttributeName (tail s)
						else
							[]
					else							
						(takeTillEqual (s)):parseAttributeName ((removeTillSpace (removeTillEqual s)))																

takeTillEqual s = takeWhile (/='=') s
						
isNotEqual c = not (c=='=')				
removeTillEqual s = if length s > 1 then if (length $ dropWhile isNotEqual s) > 1 then tail (dropWhile isNotEqual s) else []
					 else []

parseAttribute [] = ([],[])
parseAttribute s = 	if isWhiteSpace (head s) then
						parseAttribute (tail s)
					else
						if head s == '<' then
							([],s)
						else										
							((zip (parseAttributeName s) (parseAttributeValue s)),removeTillClosedTag s)


parseAttributeValue [] = []
parseAttributeValue s = 	if  isWhiteSpace (head s) then
						if length s > 1 then
							parseAttributeValue (tail s)
						else
							[]
					else 
						if take 2 s == "/>" || head s == '>' then
							[]
						else
							(takeTillClosedTagOrSpace(removeTillEqual s)):parseAttributeValue (removeTillClosedTagOrSpace s)							

parseEtag s 
			| isWhiteSpace (head $ snd s) = parseEtag (fst s,tail $ snd s)
			| otherwise = if take (1 +(length $ fst s)) (tail (dropWhile (/='/') (snd s))) == (fst s)++">" then
							(fst s , removeTillClosedTag $ drop (length $ fst s)(tail (dropWhile (/='/') (snd s))))
						  else
							parseEtag(fst s,tail (dropWhile (/='/') (snd s)))		

{- ------------- END: Task 2 -----------------}
