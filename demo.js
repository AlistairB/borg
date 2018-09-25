

// Parser Combinators - You could have invented them!













// BOSS - I want the first part and the second level domain!

var email = "alistair@email.com"

var firstPart = "alistair"
var sld = "email"


// Something like this?

var email = "alistair@email.com"

var atPosition = email.indexOf("@")
var dotPosition = email.indexOf(".")

var firstPart = email.substr(0, atPosition)
var sld = email.substr(atPosition + 1, dotPosition - atPosition - 1)






// But really it's more like this

var email = "alistair@email.com"

var atPosition = email.indexOf("@")

if (atPosition > 0) {
  var dotPosition = email.indexOf(".")

  if (dotPosition > 0) {
    var firstPart = email.substr(0, atPosition)
    var sld = email.substr(atPosition + 1, dotPosition - atPosition - 1)
  }
}





// Regex comes along

var email = "alistair@email.com"

var result = email.match(/^(.+)@(.+)\.com$/)

var firstPart = result[1]
var sld = result[2]








// More realistically

var emailRegex = /(?:[a-z0-9!#$%&'*+/=?^_`{|}~-]+(?:\.[a-z0-9!#$%&'*+/=?^_`{|}~-]+)*|"(?:[\x01-\x08\x0b\x0c\x0e-\x1f\x21\x23-\x5b\x5d-\x7f]|\\[\x01-\x09\x0b\x0c\x0e-\x7f])*")@(?:(?:[a-z0-9](?:[a-z0-9-]*[a-z0-9])?\.)+[a-z0-9](?:[a-z0-9-]*[a-z0-9])?|\[(?:(?:25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)\.){3}(?:25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?|[a-z0-9-]*[a-z0-9]:(?:[\x01-\x08\x0b\x0c\x0e-\x1f\x21-\x5a\x53-\x7f]|\\[\x01-\x09\x0b\x0c\x0e-\x7f])+)\])/


// https://emailregex.com/wp-content/uploads/sites/2/2014/06/General-Email-Regex-Railroad-Diagram-emailregex.com_.png
