/**
 * Annotates a series of examples with information about that
 */
const annotateExample = (comments, notes) => {
  for (let cn = 0; cn < notes.length; cn++) {
    const note = notes[cn];
    let comment;
    for (let ci = 0; ci < comments.length; ci++) {
      let this_comment = comments[ci];
      const match = this_comment.innerText.match(/^\(\* (\d+) \*\)$$/);
      if (match && parseInt(match[1]) == (cn + 1)) {
        comment = this_comment;
        break;
      }
    }

    if(!comment) {
      console.error("Cannot find comment for " + note.innerText);
      continue;
    }

    comment.title = note.innerText;
    comment.classList.add("tok-annotate");

    const wrapper = document.createElement("span");
    wrapper.innerText = "" + (cn + 1);

    while(comment.firstChild) comment.removeChild(comment.firstChild);
    comment.appendChild(wrapper);
  }
};

// Attempt to find examples which are
const examples = document.querySelectorAll(".annotated-example");
for (let i = 0; i < examples.length; i++) {
  const example = examples[i];

  const comments = example.querySelectorAll(".tok-comment");
  const notes = example.querySelectorAll("ol > li");
  annotateExample(comments, notes);
}
