(** Mayas HADDAD ------------------------------------------- Nassim AMGHAR *)

(** les symboles E : Epsilon
		 [a..z] : Alphabet *)
type symbole = Epsilon | A | B 

(** les opérateurs binaires *)
type bin_op = Barre | Concat

(** les opérateurs unairs *)
type un_op = Etoile

(** Par convention une expression rationnelle est un arbre binaire
    dont les noeuds sont des opérateurs et les feuilles des symboles *)
type exp = Vide | Sym of symbole | Opun of un_op*exp | Opbin of bin_op*(exp*exp)

(** Une expression rationnelle de la forme (+,(a,( *,b))) est la representation en mémoire de a+(b* ) 
    +     
   / \    
  a   *   
      |   
      b *)

(** La chaine sur laquelle on va tester la reconnaissance par la regex *)
let chaine = [A;B;A;B]

(** L'expression utilisée dans les tests, celle de l'énoncé *)
let monExpression = Opbin (Concat, (Opun (Etoile, Opbin (Barre, (Sym A, Sym B))), Sym A)) (*Opbin (Barre,(Opun (Etoile,Opbin (Barre,(Sym A,Sym B))),Sym B))*)

(** Cette fonction vaut Vrai lorsque Epsilon peut être généré par la regex en paramètre *)
let rec contient_epsilon r = match r with
			    |Sym a when a=Epsilon-> true
			    |Opun (a,b) when a=Etoile-> true
			    |Opbin (a,b) when a=Barre -> contient_epsilon (fst b) || contient_epsilon (snd b)
			    |Opbin (a,b) when a=Concat-> contient_epsilon (fst b) && contient_epsilon (snd b)
			    |_->false

(** Cette fonction vaut Vrai lorsque le caractère c peut être au début d'un mot généré par la regex en paramètre *)
let rec debut_par c r =match r with
			    |Sym a when a=c -> true
			    |Opun (a,b) when a=Etoile -> debut_par c b
			    |Opbin (a,b) when a=Barre -> debut_par c (fst b) || debut_par c (snd b)
			    |Opbin (a,b) when a=Concat-> debut_par c (fst b)
			    |_->false


(** Calcule le résidu d'une expression r par rapport à un caractère c *)
let rec residu r c = match r,c with 
			|Vide,_ -> Vide
			|_,Epsilon -> r
			|(Opun (a,b)),c when a=Etoile -> if debut_par c b then r
									else Vide
			|(Opbin (a,b)),c when a=Barre -> Opbin (Barre, (residu (fst b) c,residu (snd b) c))
			|(Opbin (a,b)),c when a=Concat-> if debut_par c (fst b) then 
										 if contient_epsilon (fst b) 						    	
										 then Opbin (Barre, (Opbin (Concat,(residu (fst b) c, (snd b))),(residu (snd b) c)))
										 else 						
										 Opbin (Concat,(residu (fst b) c, (snd b)))
										else Vide
			|Sym a,c when a=c -> Sym Epsilon
	 		|_-> Vide


(** Match le caractère avec un symbole et vaut le char correspondant *)
let string_of symbole =match symbole with
		|Epsilon -> "E"
		|A-> "a"
		|B-> "b"


(** Fait un parcours récursif infixé de l'arbre de l'expression en affichant (en parenthèsant) le symbole ou l'opération utilisé *)
let rec affiche_regex r =match r with
			|Sym a -> Printf.printf "%s" (string_of a)
			|Opun (a,b) when a=Etoile -> print_string "(";affiche_regex b;print_string ")*"
			|Opbin (a,b) when a=Barre -> affiche_regex (fst b);print_string "|";affiche_regex (snd b);
			|Opbin (a,b) when a=Concat-> affiche_regex (fst b);affiche_regex (snd b)
			|_ -> ()


(** Très utile notament (Par exemple) pour faire de l'expression Opbin (Concat, (Vide, Sym A)) --> Sym A
ou bien Eb --> b : le epsilon au début de la regex étant inutile voir nuisible lorsqu'il s'agit de résiduer l'éxpression *)
let rec normalise_regex r = match r with
			|Opbin (a,b) when (fst b)=Vide || a=Concat && (fst b)=Sym Epsilon-> normalise_regex (snd b)
			|Opbin (a,b) when (snd b)=Vide || a=Concat && (snd b)=Sym Epsilon-> normalise_regex (fst b)
			|Opun (a,b) when (b)=Vide -> Vide
			|_-> r


(** Une chaine étant une liste de caractères, cette fonction applique l'itérateur fold_left pour résiduer au fil 
du parcours gauche>droite de la liste *)
let rec residu_chaine r s = List.fold_left (fun r c -> normalise_regex (residu r c)) r s


(** Cette fonction est le prédicat qui vaut Vrai lorsque une chaine est reconnue par une regex *)
let reconnais r s = contient_epsilon (residu_chaine r s)
let monExpressionResiduee = normalise_regex (residu monExpression (A))
let monExpressionResidueeun = normalise_regex (residu monExpression (B))



(** Série de tests pour les différentes fonctions *)
let ()=
if contient_epsilon monExpression then print_string "Oui : L'expression rationnelle contient Epsilon\n"
				  else print_string "Non : Elle ne contient pas Epsilon\n";
print_string "r:";
affiche_regex monExpression;
print_string "\n";
print_string "res(r,a):";
affiche_regex monExpressionResiduee;
print_string "\n";
print_string "res(r,b):";
affiche_regex monExpressionResidueeun;
print_string "\n";
if reconnais monExpression chaine then print_string "Oui : L'expression rationnelle reconnait cette chaine \n"
				  else print_string "Non : Elle ne reconnait pas la chaine\n"
