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

const elmApp = Elm.Main.init({
    node: document.getElementById('elm')
});

function flattenCollection<T>(snapshot: QuerySnapshot<T>): T[] {
  return snapshot.docs.map(doc => doc.data());
}

firebase.auth().onAuthStateChanged(function(user) {
  if (user) {
    console.log(user)
  } else {
    console.error("signed out")
  }
});

firebase.auth().signInAnonymously().then((creds: UserCredential) => {
}).catch(function(error) {
  console.error(error)
});


// Listen From Elm

elmApp.ports.createGame.subscribe(async function (gameID: string) {
  const g = {gameID, players: {}};
  const gameRef: DocumentReference = db.collection('games').doc(gameID);
  const game = await db.runTransaction(async (t: Transaction) => {
    const gameDoc: DocumentSnapshot = await t.get(gameRef);
    if (gameDoc.exists) {
      return gameDoc.data();
    }
    t.set(gameRef, g);
    return g;
  });
  elmApp.ports.joinGame.send(game.gameID);
  console.log("game", game);
});


elmApp.ports.requestCards.subscribe(async function () {
  db.collection("white-cards").get().then((cardCollection) => {
    const cards = flattenCollection(cardCollection).map(card => ({...card, color: "white"}))
    elmApp.ports.loadCards.send(cards)
  });

  db.collection("black-cards").get().then((cardCollection) => {
    const cards = flattenCollection(cardCollection).map(card => ({...card, color: "black"}))
    elmApp.ports.loadCards.send(cards)
  });
};
