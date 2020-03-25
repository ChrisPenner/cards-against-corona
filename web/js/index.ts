import { Elm } from '../../src/Main.elm'
import firebase from '@firebase/app'
import { QuerySnapshot } from '@firebase/firestore-types'
import '@firebase/firestore'

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

db.collection("white-cards").get().then((cardCollection) => {
  const cards = flattenCollection(cardCollection).map(card => ({...card, color: "white"}))
  elmApp.ports.loadCards.send(cards)
});

db.collection("black-cards").get().then((cardCollection) => {
  const cards = flattenCollection(cardCollection).map(card => ({...card, color: "black"}))
  elmApp.ports.loadCards.send(cards)
});
