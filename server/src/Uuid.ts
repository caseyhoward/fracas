// https://stackoverflow.com/questions/105034/create-guid-uuid-in-javascript
export function generate(): string {
  var d = new Date().getTime();
  // var d2 = (performance && performance.now && performance.now() * 1000) || 0;
  var d2 = 0;
  return "xxxxxxxx-xxxx-4xxx-yxxx-xxxxxxxxxxxx".replace(/[xy]/g, function(c) {
    var r = Math.random() * 16;
    if (d > 0) {
      r = (d + r) % 16 | 0;
      d = Math.floor(d / 16);
    } else {
      r = (d2 + r) % 16 | 0;
      d2 = Math.floor(d2 / 16);
    }
    return (c === "x" ? r : (r & 0x3) | 0x8).toString(16);
  });
}
