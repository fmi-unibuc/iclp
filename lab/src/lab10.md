# Laborator 10 + 11 - Introducere in event queue/promises

Pentru exercitiile ce urmeaza puteti folosi urmatorul prototip:

```html
<!DOCTYPE html>
<html lang="en">
  <head>
  <meta charset="utf-8"/>
  </head>
  <body>
    <input type="text"></input>
    <button onclick="handler()">
      Button
    </button>
    <label id='label'>Text here</label> 
    <script>
      function change_text(text) {
        document.getElementById('label').innerHTML = text;
      }
    </script>
    <script src="main.js"></script>
  </body>
</html>
```

iar in `main.js` sa aveti implementata functia `handler`:

```js
function handler() {
    change_text("Hello there");
}
```

1. Faceti ca la fiecare apasare a butonului sa se incrementeze un numar care se va afisa (folosind functia `change_text`).

2. Faceti butonul sa activeze procesarea numerele de la 1 in sus, verificand la fiecare in parte daca este prim. Dupa ce a verificat ca numarul x este prim,
trebuie sa modificati textul folosind `change_text` sa scrieti progresul. 

Trebuie deasemenea ca interfata sa fie inca folosibila (de exemplu sa se scrie in acel `<input/>`).

Hint: ganditi-va ca faceti pauza de o secunda intre 2 numere. Dupa "eliminati" pauza.

3. Faceti ca la apasarea butonului (implicit apelul functie `handler`) sa se opreasca procesarea.

4. Alterati ce ati facut la 3 in asa fel incat procesarea sa fie reluata de unde a ramas.

5. Folosind 3 butoane si 3 handlere diferite implementati conceptul de combo:

Alegeti-va o ordine in care cele 3 trebuie apasate (de exemplu daca cele 3 butoane sunt A, B si C, atunci ordinea poate fi A, dupa B, si dupa C) si un interval 
de timp relativ scurt (de exemplu 2 secunde).

Daca utilizatorul nu apasa suficient de repede, schimbati textul in "greseala". Daca reuseste sa apese combo-ul schimbati textul in "COMBO".
Timpul incepe de la apasarea primului buton din combo.

Folositi numai setTimeout pentru sincronizare.

6. Acelasi exercitiu ca la 5, dar folosind Promises. La apasarea primului buton creati 3 `Promise` corespunzatoare fiecarui buton si rezolvati-le (`resolve`) 
cand userul apasa butonul. Cand COMBO-ul se termina, fie fiindca utilizatorul a fost prea incet sau pentru ca a reusit resetati cele 3 `Promise`s.

7. Ca la 6, dar modificati combo-ul sa poata avea oricate apasari, iar unele butoane sa poata aparea de mai multe ori in combo.


Veti folosi NodeJS pentru urmatoarele exercitii, pentru a studia cum functioneaza un event loop.

8. Creati un server (folosind `http.Server`) care raspunde la orice request cu `Hello World at /{REQUEST_PATH}` unde `{REQUEST_PATH}` este inlocuit cu path-ul din request.

9. Folosind `response.write` scrieti acest mesaj in 3 bucati.

10. Transformati serverul intr-unul de fisiere statice: cand se cere http://server/path/to/file.txt sa se downloadeze path/to/file.txt relativ la serverul care ruleaza.

    Folositi `response.write` si `fs.readFile`.

11. Nu mai cititi tot fisierul deodata in memorie, folositi un `fs.ReadStream` pentru a da raspunsul pe masura ce cititi. 
