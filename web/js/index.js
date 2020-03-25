import { Elm } from '../../src/Main.elm'
import firebase from '@firebase/app'
import '@firebase/firestore'

const fbApp = firebase.initializeApp({
  apiKey: 'AIzaSyBI_jYegnxS3IHK2RzNVkbrdXW4zdylvaw',
  authDomain: 'localhost',
  projectId: 'cards-against-corona'
});

const db = fbApp.firestore()



db.collection("white-cards").get().then((querySnapshot) => {
    querySnapshot.forEach((doc) => {
        console.log(doc.data())
        // console.log(`${doc.id} => ${doc.data()}`);
    });
});

  // apiKey?: string;
  // authDomain?: string;
  // databaseURL?: string;
  // projectId?: string;
  // storageBucket?: string;
  // messagingSenderId?: string;
  // appId?: string;
  // measurementId?: string;
// })


const elmApp = Elm.Main.init({
    node: document.getElementById('elm')
});


