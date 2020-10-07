{- Savu Ioana Rusalda 
        325CB
-}
{-
  OBSERVATIE IMPORTANTA!!
  In urma rularii checker-ului pe propria masina virtuala, acesta avea un comportament ciudat.
  Acest lucru se poate datora memoriei pe care masina virtuala o pune la dispozitie(in cazul meu
  limitata).
  Semnalez acest lucru pentru a fi luat in considerare in cazul unui fail din partea checker-ului.
  In ceea ce am observat, in urma unor rulari consecutive, au existat si momente in care acesta a 
  functionat corect. Pe forum am atasat si un print-screen care surpinde acest comportament.

  Multumesc!
-}
module Query where

import Data.List
import Movie
import Rating
import UserInfo

type Column = String
type TableSchema = [Column]
type Field = String
type Entry = [Field]

data Table = Table TableSchema [Entry]

type ColSeparator = Char
type LnSeparator = Char

--splitBy despatre un string dupa un delimitator
splitBy :: Char -> String -> [String]
splitBy c = foldr op [[]]
              where op x (y:ys)
                      | x /= c = (x:y):ys
                      | otherwise = []:(y:ys)

--splitCol separa fiecare linie in coloane dupa delimitaatorul de coloane
splitCol :: Char -> [String] -> [[String]]
splitCol separator [] = []
splitCol separator (x : xs) = (splitBy separator x) : (splitCol separator xs)

{-generate_TableSchema generaza headerul tabelei 
  *pentru header este nevoie doar de primul element al listei returnate in 
    urma separarii coloanelor
  *pentru entries este nevoie de restul elementelor din rezultat
  *cu ajurorul comenzii lines stringul este despartit dupa '\n'
-}
generate_TableSchema :: Char -> String -> [String]
generate_TableSchema separator str = head $ splitCol separator (lines str)

generate_entry :: Char -> String -> [[String]]
generate_entry separator str = tail $ splitCol separator (lines str)

--read_table genereaza tabela apeland functiile anterioare pentru fiecare componenta
read_table :: ColSeparator -> LnSeparator -> String -> Table
read_table s1 s2 str = (Table (generate_TableSchema s1 str) (generate_entry s1 str))

--fiecare tabel este creat in functie de delimitatorii proprii
user_info = read_table '|' '\n' user_info_str
rating = read_table ' ' '\n' rating_str
movie = read_table '|' '\n' movie_str

--nr_col determina numarul de coloane din tabela
--nr_col :: Table -> Int
--nr_col (Table header entries) = length header

--length_field determina numarul de caractere al fiecarui
length_field l = map (length) l
--update_max realizeaza maximul dintre 2 liste
update_max l1 l2 = zipWith max l1 l2

--find_max primeste 2 liste si returneaza o lista cu elementele care au cele mai multe caractere
find_max [] l2 = l2
find_max l1 [] = l1
find_max l1 l2 = zipWith (\x y-> if length x > length y then x else y) l1 l2

--max_col face maximul dintre header si maximul rezultat pe lista de enries
max_col :: Table -> [Int]
max_col (Table header entries) = update_max (length_field (foldr find_max [] entries)) 
                                                                               (length_field header)

--chr_need determina cate caractere '-' trebuie folosite pentru a crea marginea capatului de tabel
chr_need :: Table -> Int
chr_need table = (foldl (+) 0 (max_col  table)) + (nr_col table) + 1 
                 where 
                     nr_col (Table header entries) = length header 

--line_delimiter creeaza marginea tabelului din caractere de tipul '-'
line_delimiter nr str
 |nr == 0 = str ++ "\n"
 |otherwise = (line_delimiter (nr-1) (str ++ "-") )

{-space_need calculeaza cate spatii are nevoie fiecare camp pentru a ajunge la dimensiunea maximului
   de pe coloana. Rezultatul este intors sub forma unei liste-}
space_need max line = zipWith (-) max (length_field line)

--generate_space adauga fiecarui camp necesarul de spatii
generate_space :: [Int] -> [[Char]] -> [[Char]]
generate_space [] [] =[]
generate_space (x:xs) (y:ys) = (y ++(take x (repeat ' '))) : (generate_space xs ys)

--write_line adauga caracterul '|' fiecarui camp (delimitator coloane)
write_line :: [[Char]] -> [Char]
write_line []       = ""
write_line (x : xs) = x ++"|" ++ (write_line xs )

--generate_line creaza stringul corespunzator unei linlii din tabe
generate_line :: [[Char]] -> [Int] -> [Char]
generate_line line max_per_col = "|" ++ (write_line $ 
                                            generate_space (space_need max_per_col line) line)++"\n"

--create_header realizeaza un string ce contine delimitatoarele headerului si headerul
create_header :: Table -> [Char]
create_header table@(Table header entries) = (line_delimiter (chr_need table) "") ++ 
                                            (generate_line header (max_col table)) ++ 
                                            (line_delimiter (chr_need table) "")


--create_entries creeaza stringul corespunzator tuturor liniilor
create_entries :: Table -> [Int] -> [Char]
create_entries table@(Table header []) nr = ""
create_entries table@(Table header (x:xs)) nr = (generate_line x nr) ++ 
                                                                (create_entries(Table header xs) nr)

{-create_table formeaza stringul corespunzator unui tabel prin concatenarea stringurilor rezultate 
  din functiile anterioare.(header + entires + "---------")-}
create_table :: Table -> [Char]
create_table tabel = (create_header tabel) ++ (create_entries tabel (max_col tabel)) ++ 
                                                                (line_delimiter (chr_need tabel) "")

instance Show Table where
    show (Table header entries) =  create_table (Table header entries)

--convert transforma un string in integer
convert :: String -> Integer
convert x = read x ::Integer

--getValue convertsete un Maybe Int la Int
getValue :: Maybe Int -> Int
getValue Nothing  = -1
getValue (Just x) = x

--find_col returneaza idicele corespunzator coloanei cautate
find_col :: Column -> Table -> Int
find_col col (Table header entries) = getValue $ elemIndex col header

--removeItems elimina elementul de pe pozitia x din lista
removeItems _ [] = []
removeItems x (y:ys) | x /= y    = removeItems x ys
                     | otherwise = y : removeItems x ys

{-removeCol elimina coloanele diferite de coloana cautata     
  select_col selecteaza o coloana dintr-un tabel
-}        
select_col col table@(Table header entries) = (Table (removeItems col header) 
                                                          (removeCol (find_col col table) entries))
                                 where 
                                 removeCol _ [] = []
                                 removeCol index (x:xs) = ((x !! index) : []) : (removeCol index xs)

{-concat_tables concateneaza 2 tabele
  *concat_entries concateneaza entries
  *concat_header concateneaza headere
-}
concat_tabels :: Table -> Table -> Table
concat_tabels (Table h1 e1) (Table h2 e2) = (Table (concat_headers h1 h2) (concat_entries e1 e2))
                                where
                                concat_entries [] [] = []
                                concat_entries _ []  = []
                                concat_entries [] _  = []
                                concat_entries (x:xs) (y:ys) = ((x++y):[]) ++ (concat_entries xs ys)

                                concat_headers h1 [] = h1
                                concat_headers [] h2 = h2
                                concat_headers h1 h2 = concat_headers (init h1) ((last h1):h2)

--select manyCol construieste tabelul format din coloanele trimise ca parametru
select_manyCol :: [Column] -> Table -> Table
select_manyCol [x] table = (select_col x table)
select_manyCol (x:xs) table =  concat_tabels (select_col x table) (select_manyCol xs table)


--select_specificCol returneaza un tabel format din coloana cautata si nr de randuri specificat
select_specificCol col nr table@(Table header entries) = (Table (removeItems col header) 
                                                (removeSpecificCol nr (find_col col table) entries))
       where
       removeSpecificCol _ _ [] = []
       removeSpecificCol 0 _ _ = []
       removeSpecificCol nr index (x:xs) = ((x !! index) : []) : (removeSpecificCol (nr-1) index xs)

{-select_manyCol_specific construieste tabelul format din coloanele trimise ca parametru cu un numar
  specific de randuri
-}
select_manyCol_specific nr [x] table = (select_specificCol x nr table)
select_manyCol_specific nr (x:xs) table@(Table header entries) =  
                 concat_tabels (select_specificCol x nr table) (select_manyCol_specific nr xs table)

{-select_Lt seleteaza doar acele entries care au elemente exclusiv mai mic​ decât o valoare data
  *verify_Lt returneaza intrarile care indepliniesc conditia data
-}
select_Lt :: Column -> Integer -> Table -> Table
select_Lt col nr table@(Table header entries) = 
                                          (Table header (verify_Lt (find_col col table) nr entries))
                                        where
                                        verify_Lt _ _ [] = []
                                        verify_Lt index nr (x:xs)
                                         | (convert (x !! index)) < nr = x:(verify_Lt index nr xs)
                                         | otherwise = (verify_Lt index nr xs)

{-select_Lt seleteaza doar acele entries care au elemente egale cu o valoare data
  *verify_Eq returneaza intrarile care indepliniesc conditia de egalitate
-}
select_Eq :: Column -> Field -> Table -> Table
select_Eq col val table@(Table header entries) = 
                                         (Table header (verify_Eq (find_col col table) val entries))
                                         where
                                          verify_Eq _ _ [] = []
                                          verify_Eq index val (x:xs)
                                           | (x !! index) == val = x:(verify_Eq index val xs)
                                           | otherwise = (verify_Eq index val xs)

{-select_In seleteaza doar acele entries care se afla in intervalul specificat
  *verify_In returneaza intrarile care indepliniesc conditia data
-}
select_In col interval table@(Table header entries) = 
                                    (Table header (verify_In (find_col col table) interval entries))
                                    where
                                    verify_In _ _ [] = []
                                    verify_In index interval (x:xs)
                                      | elem (x !! index) interval = x:(verify_In index interval xs)
                                      | otherwise = (verify_In index interval xs)

{-select_notLt seleteaza doar acele entries care sunt mai mari decat o valoare data
  *verify_notLn returneaza intrarile care indepliniesc conditia data
-}                                     
select_notLt :: Column -> Integer -> Table -> Table
select_notLt col nr table@(Table header entries) = 
                                       (Table header (verify_notLt (find_col col table) nr entries))
                                    where
                                    verify_notLt _ _ [] = []
                                    verify_notLt index nr (x:xs)
                                      | (convert (x !! index)) >= nr = x:(verify_notLt index nr xs)
                                      | otherwise = (verify_notLt index nr xs)

{-not_filter face diferenta intre 2 tabele (cel initial si cel rezultat in urma aplicarii 
    unui filtru)
  *diference_entry elimina din intrarile initiale intrarile tabelului 2
-}
not_filter :: Table -> Table -> Table
not_filter (Table h e)(Table newH newE) = (Table newH (diference_entries e  newE))
                                where
                                diference_entries e1 [] = e1
                                diference_entries [] _ = []
                                diference_entries (x:xs) newentries
                                  |elem x newentries == False = x :(diference_entries xs newentries)
                                  |otherwise = (diference_entries xs newentries )

--reunion realizeaza reuniunea dintre 2 tabele
reunion :: Table -> Table -> Table
reunion (Table header1 entries1) (Table header2 entries2) = (Table header1 (entries1 ++ entries2))


data FilterCondition = Lt Field Integer | Eq Field String | In Field [String] | Not FilterCondition
-- TODO 3
--getFilter :: FilterCondition -> TableSchema -> (Entry -> Bool)
--getFilter = undefined

data Query = Filter FilterCondition Query |
             Select [String] Query |
             SelectLimit [String] Integer Query |
             Cosine Query |
             Query :|| Query |
             Atom Table

eval :: Query -> Table
eval (Atom table) = table
eval (Select col x) = (select_manyCol  col (eval x))
eval (SelectLimit col nr (Atom table)) = (select_manyCol_specific nr col table)
eval (Filter (Lt col nr) table) = (select_Lt col nr (eval table))
eval (Filter (Eq col val)  table) = (select_Eq col val (eval table))
eval (Filter (In col interval) table) = (select_In col interval (eval table))
eval (Filter (Not (Lt col nr)) table)= (select_notLt col nr   (eval table))
eval (Filter (Not (In col interval)) table) = (not_filter (eval table) 
                                                              (select_In col interval (eval table)))
eval (Filter (Not(Eq col val)) table) = (not_filter (eval table) (select_Eq col val (eval table)))
eval (table1 :|| table2) = reunion (eval table1) (eval table2)


--return_zone returneaza zona unui anumit user
return_zone :: String -> Field
return_zone user_id = head ( head e )
    where
        (Table h e)=(eval $ Select ["zone"] $ Filter (Eq "user_id" user_id)$ Atom user_info)

same_zone :: String -> Query
same_zone user_id = Select ["user_id" , "occupation"] $  
                    Filter (Not (Eq "user_id" user_id)) $
                    Filter (Eq "zone" (return_zone user_id)) $
                    Atom user_info

male_within_age :: Integer -> Integer -> Query
male_within_age inf sup = Select ["occupation" , "zone"] $ 
                          Filter (Not (Lt "age" (inf + 1))) $ 
                          Filter (Lt "age" sup) $
                          Filter (Eq "sex" "M")$ 
                          Atom user_info

mixed :: [String] -> [String] -> Int -> Query
mixed zone occupation age =   Select ["user_id"]$ 
                              Filter (In "zone" zone) $ 
                              Filter (In "occupation" occupation) $ 
                              Filter (Lt "age" (toInteger age)) $ 
                              Atom user_info
 