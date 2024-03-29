import { Elm } from '../../src/Main.elm'
import firebase from '@firebase/app'
import { UserCredential } from '@firebase/auth-types'
import { QuerySnapshot, DocumentSnapshot, DocumentReference, Transaction} from '@firebase/firestore-types'

// Imported for side effects
import '@firebase/firestore'
import '@firebase/auth'

if (module.hot) {
  module.hot.dispose(() => {
    window.location.reload();
  });
}

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

interface Card {
  text: string;
  color: string;
}

interface Assets {
  whiteCards: Card[];
  blackCards: Card[];
}


interface Round {
  submissions: {[userID: string]: Card[]};
  blackCard: Card;
}


interface Game {
  gameID: string;
  players: {[userID: string]: Player};
  turn: string;
  whiteDeck: Card[];
  blackDeck: Card[];
  round: Round;
}

interface UploadPlayer {
  gameID: string;
  player: Player;
  submission: Card[];
}


interface Flags {
  userID : string;
  assets : Assets;
}

async function init() {
  const {user} : UserCredential = await firebase.auth().signInAnonymously();
  if (user) {
    console.log(user)
  } else {
    console.error("signed out")
  }

  const [whiteCards, blackCards] : Card[][] = await Promise.all([
      db.collection("white-cards")
        .get().then(flattenCollection)
        .then(cards => cards.map(card => ({color: 'white', ...card}))),
      db.collection("black-cards")
        .get().then(flattenCollection)
        .then(cards => cards.map(card => ({color: 'black', ...card}))),
  ] as Promise<Card[]>[]);

  const flags : Flags = {
    userID: user.uid,
    assets: {whiteCards, blackCards},
  }
  const elmApp = Elm.Main.init({
    node: document.getElementById('elm'),
    flags: flags,
  });


  // Listen From Elm
  elmApp.ports.createOrJoinGame.subscribe(async function ({game, player}) {
    const gameRef: DocumentReference = db.collection('games').doc(game.gameID);
    game = await db.runTransaction<Game>(async (t: Transaction) => {
      const gameDoc: DocumentSnapshot = await t.get(gameRef);
      if (gameDoc.exists) {
        game = gameDoc.data();
        if (game.players[player.playerID]) {
          return game;
        }
        game.players[player.playerID] = player;
        t.update(gameRef, {[`players.${player.playerID}`]: player});
        return game as Game;
      }
      t.set(gameRef, game);
      return game;
    });
    elmApp.ports.downloadGame.send(game);
    gameRef.onSnapshot(function(gameDoc) {
      elmApp.ports.downloadGame.send(gameDoc.data());
    });
  });

  elmApp.ports.uploadGame.subscribe(async function ({players, gameID, ...rest}: Game) {
    let syncData = {};
    Object.entries(rest).forEach(([key, value]) => {
      if (value) {
        syncData[key] = value;
      }
    });
    if (players) {
      Object.entries(players).forEach(([playerID, value]) => {
        if (value) {
          syncData[`players.${playerID}`] = value;
        }
      });
    }
    const gameRef: DocumentReference = db.collection('games').doc(gameID);
    gameRef.update(syncData);
  });
};
init();

// function drawBlackCard(game: Game, blackCards: Card[]): Game {
//   const deck = game.blackDeck;
//   const [first, rest] = [deck[0], deck.slice(1)];
//   const newRound = {
//     ...game.round,
//     blackCard: first,
//   };
//   let blackDeck = rest;
//   if (rest.length === 0) {
//     blackDeck = blackCards;
//   }
//   return {
//     ...game,
//     round: newRound,
//     blackDeck: blackDeck,
//   };
// }
