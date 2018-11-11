structure ThreeLetter =
struct

  fun droplast l = List.take (l, List.length l - 1)

  fun chop s = String.concat (map Char.toString (droplast (String.explode s)))

  fun getWords wordlist =
  let
    val file = TextIO.openIn wordlist
    fun addlines lines = 
      case TextIO.inputLine file of 
            SOME line => addlines (line::lines)
          | NONE => rev lines
    val words = map chop (addlines [])
  in
    droplast words
  end

  val scrabbleWords = getWords "tlws.txt"
  val increpareWords = getWords "increpare.txt"

  (* only works if chars is a list of characters of length 3 *)
  fun permute3 chars =
    case chars of
         [x, y, z] => [[x, y, z], [x, z, y], [y, x, z], [y, z, x], [z, x, y], [z, y, x]]
       | _ => [chars]

  fun anagrams s =
  let
    val chars = String.explode s
    val permutes = permute3 chars
    val anagrams = map (String.concat o (map Char.toString)) permutes
  in
    anagrams
  end

  fun member x l = List.exists (fn y => y = x) l

  fun isWord dictionary w = member w dictionary

  fun checkAnagramMembership dictionary word =
    let
      val anas = anagrams word
    in
      List.all (isWord dictionary) anas
    end

  fun findAnagrams dict =
    List.filter (checkAnagramMembership dict) dict 

end
