import { Elm } from '../../src/Main.elm'
import firebase from '@firebase/app'
import { UserCredential } from '@firebase/auth-types'
import { QuerySnapshot, DocumentSnapshot, DocumentReference, Transaction} from '@firebase/firestore-types'

// Imported for side effects
import '@firebase/firestore'
import '@firebase/auth'

const fbApp = firebase.initializeApp({
  apiKey: 'AIzaSyBI_jYegnxS3IHK2RzNVkbrdXW4zdylvaw',
  authDomain: 'localhost',
  projectId: 'cards-against-corona'
});

const db = fbApp.firestore()

function flattenCollection<T>(snapshot: QuerySnapshot<T>): T[] {
  return snapshot.docs.map(doc => doc.data());
}

interface Player {
  playerID: string;
}

interface Game {
  gameID: string;
  players: {[playerID: string]: Player};
  turn: string;
}

interface Flags {
  user : Player;
}

async function init() {
  const {user} : UserCredential = await firebase.auth().signInAnonymously();
  if (user) {
    console.log(user)
  } else {
    console.error("signed out")
  }

  const elmApp = Elm.Main.init({
    node: document.getElementById('elm'),
    flags: {user: {playerID: user.uid}} as Flags,
  });


  // Listen From Elm
  elmApp.ports.createOrJoinGame.subscribe(async function (newGame : Game) {
    const gameRef: DocumentReference = db.collection('games').doc(newGame.gameID);
    const game = await db.runTransaction<Game>(async (t: Transaction) => {
      const gameDoc: DocumentSnapshot = await t.get(gameRef);
      if (gameDoc.exists) {
        return gameDoc.data();
      }
      t.set(gameRef, newGame);
      return newGame;
    });
    elmApp.ports.joinedGame.send(game);
    console.log("game", game);
  });


  const [whiteCards, blackCards] = await Promise.all([
      db.collection("white-cards").get().then((cardCollection) => flattenCollection(cardCollection).map(card => ({...card, color: "white"}))),
      db.collection("black-cards").get().then((cardCollection) => flattenCollection(cardCollection).map(card => ({...card, color: "black"}))),
  ]);
  elmApp.ports.loadedCards.send([...whiteCards, ...blackCards])
};
init();

