'use strict'

function _random (max) {
  return Math.round(Math.random() * 1000) % max
}

export class Store {
  constructor  () {
    this.data = []
    this.selected = undefined
    this.id = 1
  }
  buildData (count = 1000) {
    const adjectives = ["pretty", "large", "big", "small", "tall", "short", "long", "handsome", "plain", "quaint", "clean", "elegant", "easy", "angry", "crazy", "helpful", "mushy", "odd", "unsightly", "adorable", "important", "inexpensive", "cheap", "expensive", "fancy"]
    const colours = ["red", "yellow", "blue", "green", "pink", "brown", "purple", "brown", "white", "black", "orange"]
    const nouns = ["table", "chair", "house", "bbq", "desk", "car", "pony", "cookie", "sandwich", "burger", "pizza", "mouse", "keyboard"]
    const data = []
    for (let i = 0; i < count; i++)
      data.push({id: this.id++, label: adjectives[_random(adjectives.length)] + " " + colours[_random(colours.length)] + " " + nouns[_random(nouns.length)] })
    return data
  }
  updateData (mod = 10) {
    for (let i=0; i<this.data.length; i += 10) {
      this.data[i] = Object.assign({}, this.data[i], { label: this.data[i].label + ' !!!' })
    }
  }
  delete (id) {
    const idx = this.data.findIndex(d => d.id == id)
    this.data.splice(idx, 1)
  }
  run  () {
    this.data = this.buildData()
    this.selected = undefined
  }
  add () {
    this.data = this.data.concat(this.buildData(1000))
  }
  update () {
    this.updateData()
  }
  select (id) {
    this.selected = id
  }
  runLots () {
    this.data = this.buildData(10000)
    this.selected = undefined
  }
  clear () {
    this.data = []
    this.selected = undefined
  }
  swapRows () {
    if(this.data.length > 10) {
      const a = this.data[4]
      this.data[4] = this.data[9]
      this.data[9] = a
    }
  }
}