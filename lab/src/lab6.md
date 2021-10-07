# Intorucere in Elixir/Erlang

1. Avand la dispozitie un character list (delimitat prin ' ', spre deosebire de `String`) ce reprezinta un ADN, transformati-l in RNA-ul corespunzator.

   ADN-ul este un sir de 'A', 'C', 'G' si 'T', iar RNA-ul un sir de  'A', 'C', 'G', 'U'. RNA-ul corespunzator se obtine transformand fiecare litera in corespondentul sau:

   * A se transforma in U
   * C se transforma in G
   * G se transforma in C
   * T se transforma in A 

2. Avand la dispozitie un text si un cuvant numarati aparitiile cuvantului in acel text.

3. Acelasi lucru ca la 2, doar ca cuvintele se compara case insensitive ('a' este la fel ca 'A').

4. Ca la 2 si 3 doar ca se doreste un map care numara aparitiile fiecarui cuvant din text:

   Daca textul este `"Ana are mere, si uneori are si pere"` rezultatul este: `%{"Ana" => 1, "are" => 2, "mere" => 1, "si" => 2, "uneori" => 1, "pere" => 1}`

    Puteti folosi orice functie doriti din standard library [String](https://hexdocs.pm/elixir/String.html) sau [Enum](https://hexdocs.pm/elixir/Enum.html#functions)
