# Proiecte

Pentru proiectul final va trebui sa implementati o interfata client/server. Server-ul si client-ul vor fi procese separate, care comunica printr-un socket TCP.

Serverul va trebui implementat folosind unul din cele conceptele, iar clientul altul. Cele 4 concepte de la curs sunt:

 - Queues/mutex/Thread Pool - Java (primele 3 cursuri)
 - Simple Transactional Memory - C++/Haskell/C#/orice limbaj care are suport decent pentru STM-uri printr-o librarie 
 - Lucrul cu actori, supervisori - Erlang/Elixir 
 - Event loop/futures - Python/Javascript(Typescript)

Pentru toate proiectele trebuie sa aveti in vedere urmatoarele detalii:

 - Trebuie sa se poata interactionea cumva cu clientii pentru a-i controla. De exemplu sa se dea comenzi de la standard input, sau, daca doriti in mod special, chiar o interfata grafica. 
    - E important ca clientii sa fie concurenti. Cat timp se citesc comenzi, notificarile de la server trebuiesc in continuare citite de exemplu.
 - Server-ul trebuie sa fie rezistent la erori. Daca clienti se deconecteaza in mijlocul unor operatii, trimit mesaje aiurea, sau alte actiuni similare, server-ul trebuie 
   in continuare sa poata satisface alti clienti.
 - Clientii trebuie sa fie rezistenti la erori. Este ok daca clientii se opresc atunci cand nu se pot conecta la server, dar daca primesc mesaje de eroare de anumite feluri 
   sau raspunsuri in alt format, conexiunea tot trebuie tinuta in viata.
 - Server-ul trebuie sa fie concurent. Cat timp execute operatii legate doar de un client, nu ar trebui sa fie blocat din a face operatii legate de alti clienti.
 - La solutia cu STM-uri, fiecare operatie atomica trebuie sa aiba o singura tranzactie pe server.
 - Puteti folosi librarii care sa va ajuta cu comunicarea dintre client si server (desi in general toate limbajele au suport pentru server/client TCP direct in standard library).
 
Proiectele pe care le puteti alege sunt urmatoarele. Mai multi oameni pot alege acelasi proiect atata timp cat au alta combinatie de concepte de concurenta pentru server si/sau client.

1. Server/client E-MAIL
   Serverul si clientul comunica cu urmatoarele mesaje. Pentru simplitate puteti face ca fiecare mesaj sa se termine cu `\n`:

   Client -> Server:
     - `CREATE_ACCOUNT username password `
        - Raspunsul e fie
            - `OK`
            - `ERROR motiv`
     - `LOGIN username password`
        - Raspunsul e fie 
            - `OK`
            - `ERROR motiv`
     - `LOGOUT`
        - Raspunsul e fie 
            - `OK`
            - `ERROR motiv`
     - `SEND user1 user2 user3 ... mesaj`
        - Raspunusl e fie 
            - `OK`
            - `ERROR motiv`
     - `READ_MAILBOX`
        - Raspunsul e fie 
            - `OK id1 id2 id3 .. idN`
            - `ERROR motiv`
     - `READ_MSG id`
        - Raspunusl e fie 
            - `OK expeditor mesaj`
            - `ERROR mesaj`

   Pe deasupra serverul va trimite notificari clientului, nu va astepta un raspuns de la acesta:
    - `FORCE_LOGOUT`
    - `NEW_MESSAGE_IN_MAILBOX id`

   Un client in general se va conecta la server, isi va crea un cont (cu `CREATE_ACCOUNT`) daca nu are inca, se va loga cu `LOGIN` si va trimite/citi mesaje. Cand va termina cu 
   serviciul de e-mail se va deloga (folosind `LOGOUT`), si (daca vrea) client-ul poate crea alte conturi/loga cu alt cont.

   Server-ul trebuie sa garanteze ca un user nu poate fi logat de pe 2 clienti deodata. Cand un al doilea client se conecteaza la server cu `LOGIN`, serverul ar trebui sa il deconecteze 
   pe primul client cu `FORCE_LOGOUT`. 

   Cand un user trimite un mesaj cu `SEND` server-ul trebuie sa verifice ca exista toti acei destinatari. Daca macar unul nu exista mesajul TREBUIE sa nu fie trimis, si o eroare trebuie trimisa 
   user-ului care a incercat sa trimita mesajul. Daca un user este logat in momentul in care cineva ii trimite un mesaj, serverul trebuie sa il notifice, folosind mesajul 
   `NEW_MESSAGE_IN_MAILBOX id`.

   `READ_MAILBOX` trebuie sa intoarca o lista de id-uri (care pot fi ce doriti voi, numere, stringuri), care reprezinta identificatori pentru mesajele primite de acel user. 
   Folosind un `id` si mesajul `READ_MSG` clientul trebuie sa poata citi mesajul corespunzator.

   La orice probleme (nume utilizator/id invalid, comanda invalida, etc) server-ul trebuie sa ii scrie clientului un motiv pentru eroara respectiva.

2. Server/client File sharing

   Serverul si clientul comuinica cu urmatoarele mesaje. Pentru simplificate puteti face ca fiecare mesaj sa se termine cu "\n":

   Client -> Server 
    - `CREATE_FOLDER /path/to/folder`
        - Raspunusl e fie 
            - `OK`
            - `ERROR motiv`
    - `READ_FILE /path/to/file`
        - Raspunsul e  fie 
            - `OK continut`
            - `ERROR motiv`
    - `WRITE_FILE /path/to/file content`
        - Raspunusl e fie 
            - `OK`
            - `ERROR motiv`
    - `DELETE /path/to/file_or_folder`
    - `MOVE /path/to/file_or_folder /new_path/to/file_or_folder`
        - Raspunsul e fie 
            - `OK`
            - `ERROR motiv`
    - `STOP_MODIFICATIONS /path/to/file_or_folder`
    - `START_MODIFICATIONS /path/to_file_or_folder`
        - Raspunsul e fie  
            - `OK`
            - `ERROR motiv`
    - `START_NOTIFICATIONS`
    - `END_NOTIFICATIONS`
        - Raspunusl e doar 
            - `OK`
    Toate aceste operatii trebuie sa fie atomice din perspectiva unui user. Un user nu trebuie sa vada modificari partiale ale acestui system. De exemplu daca se sterge un folder, 
    `/A` in care exista fisierele `B` si `C`, atunci daca citirea lui `/A/B` esueaza (fiindca nu mai exista fisierul), trebuie sa esueze si citirea lui `/A/C` imediat de dupa 
    (presupunand ca nu se mai executa alta operatie).

    Serverul trebuie sa suporte mai multi clienti concomitenti. Cand vreun client foloseste comanda STOP_MODIFICATIONS pe un folder/fisier, atunci orice operatie care implica modificari 
    pe ceva din acel fisier/director (cum ar fi `WRITE_FILE`, `CREATE_FOLDER` in vreun sub-folder, `DELETE` pe fisier sau pe vreun sub-folder, sau `MOVE` oriunde mai jos sau pe vreun parinte)
    trebuie sa esueze cu un motiv corespunzator.

    Pentru simplitate puteti presupune ca nu pot exista 2 locatii, una cuprinsa in alta, amandoua cu `STOP_MODIFICATIONS` activ. La orice moment de timp, un client (altul decat cel care a trimis comanda 
    `STOP_MODIFICATIONS /path`) poate sa trimita comanda `START_MODIFICATIONS /path` pentru a permite din nou modificari.

    Cand un client trimite `START_NOTIFICATIONS`, serverul va incepe sa-i trimita notificari cu toate operatiile ce se executa de vreun client pe acest server, cu exceptia `START_MODIFICATIONS` si `END_NOTIFICATIONS`
    pana acest client va trimite `END_NOTIFICATIONS`.
    O notificare are forma `NOTIFICATION OP` unde `OP` este o operatie care s-a executat.

    La orice operatie serverul va notifica doar clientii care si-au activat notificarile, nu si pe ceilalti.

    La orice eroare, serverul trebuie sa justifice eroarea cu un motiv ( `file does not exist, `stop_modifications active`, etc).

3. Joc de carti simplificat - EASYPOKER.

   Un server care mentine diferite camere in care jucatorii joaca EASYPOKER:
    - Un joc cu `52` de carti 
        - Orice carte are un numar (de la `1` la `13`) si o culoare (`Rosu`, `Galben`, `Albastru`, `Verde`)
    - Fiecare jucator trage 6 carti 
    - Castiga cea mai mare combinatie de `4` carti cu acelasi numar. Daca nu exista niciun jucator cu `4` carti cu acelasi numar, atunci castiga cea mai 
      mare combinatie de `3` carti cu acelasi numar. Daca iarasi nu exista, atunci castiga cea mai mare pereche (`2` carti cu acelasi numar), si daca nici asa 
      nu exista niciun jucator atunci nu castiga nimeni.
    - Intr-o joc pot fi maxim $6$ oameni.
    - Jocul e o repetitie de $3$ runde
        1. Se impart cartile aleator, fiecare jucator va vedea doar cartile lui deocamdata
        2. Fiecare jucator pariaza un numar de credite, independent de toti ceilalti jucatori. Toti pariaza deodata, nu exista o ordine anume.
        3. Toti jucatorii isi arata cartile, si castigatorul (sau castigatorii daca sunt mai multi) isi impart creditele puse in joc pana acum intre ei.
    - Daca nu castiga nimeni o runda, creditele se acumuleaza pana la urmatoarea serie de $3$ runde.

    Va trebui sa implementati un server care poate mentine mai multe "camere" de joc, si un client capabil sa joace in mai multe camere deodata.

    Serverul si clientul comunica prin urmatoarele mesaje:

    Client -> Server:
    - `CREATE_ROOM room_name min_bet`
        - Raspunsul e fie 
            - `OK`
            - `ERROR motiv`.
    - `JOIN_ROOM room_name`
        - Raspunsul e fie 
            - `OK`
            - `ERROR motiv`
    - `LEAVE_ROOM room_name`
        - Raspunsul e fie 
            - `OK`
            - `ERROR motiv`
    - `BET room_name credite`
        - Raspunsul e fie 
            - `OK`
            - `ERROR motiv`

    Server -> Client:
    - `GAME STARTED room_name total_credite cartea1 cartea2 cartea3 cartea4 cartea5 cartea6`
        - `carte1/2/3/4/5/6` sunt in formatul `numarINITIALA`, de exemplu `13G` pentru cartea cu numarul 13 si culoarea Galbena 
        - Nu asteapta un raspuns din partea clientului
    - `GAME_WON room_name credite_castigate`
    - `GAME_LOST room_name credite_pierdute`

    Fiecare camera create folosind `CREATE_ROOM` are asociat un numar minim the credite ce pot fi pariate in runda `2` a jocului.

    Imediat ce sunt cel putin 2 jucatori intr-o camera, serverul va genera cartile corespunzatoare fiecarui jucator si va trimite fiecarui jucator informatii despre joc 
    folosind mesajul `GAME STARTED ...`. Fiecare joc va dura 30 de secunde, sau pana cand toti participantii au trimis mesajul `BET ...` pentru aceasta camera. 
    Daca un jucator nu trimite un mesaj `BET` valid pana la terminarea celor 30 de secunde, se presupune ca trimite numarul minim de credit (parametrul `min_bet` folosit la creeea camerei).

    Imediat ce se termina runda de pariuri, serverul va trimite fiecarui jucator rezultatul jocului.

4. Va urma...
