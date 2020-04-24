import firebase from '@firebase/app'

// Imported for side effects
import '@firebase/firestore'
import '@firebase/auth'

import cards from "./ignore/cards.json"


const fbApp = firebase.initializeApp({
  apiKey: 'AIzaSyBI_jYegnxS3IHK2RzNVkbrdXW4zdylvaw',
  authDomain: 'localhost',
  projectId: 'cards-against-corona'
});

const db = fbApp.firestore()


const blackCards = db.collection("/black-cards")
const whiteCards = db.collection("/white-cards")


async function resetDB() {
  let batch = db.batch()
  let q = await blackCards.get()
  console.log("Deleting black cards...");
  q.forEach( d => {
    batch.delete(d.ref)
  })
  await batch.commit()

  batch = db.batch()
  cards.blackCards.forEach(card => {
    batch.set(blackCards.doc(), card)
  })
  console.log("Uploading black cards...");
  await batch.commit()

  batch = db.batch()
  console.log("Deleting white cards...");
  q = await whiteCards.get()
  q.forEach( d => {
    batch.delete(d.ref)
  })
  await batch.commit();

  batch = db.batch()
  cards.whiteCards.forEach(text => {
    batch.set(whiteCards.doc(), {text})
  })
  console.log("Uploading white cards...");
  await batch.commit();

  console.log("DONE!");
}

resetDB();
