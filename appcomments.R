# Comments ----
panel.comment <-
  fluidPage(
    br(),
    br(),
    h4(
    strong(
      "If you have any questions or suggestions regarding our online oligo design tool for PCR tagging, please leave us a comment here or email us."
    )
  ),
  h4(
    strong(
      "If you want to comment as a guest, just type the comment, enter your name and email and tick the checkbox 'I'd rather post as guest'."
    )
  ),
  br(),
  br(),
  div(
    id = "disqus_thread",
    HTML(
      "<script>
      var disqus_config = function () {
      this.page.url = 'http://www.genetagging.com/';
      this.page.identifier = 'genetagging';
      };
      (function() {
      var d = document, s = d.createElement('script');
      s.src = 'https://genetagging.disqus.com/embed.js';
      s.setAttribute('data-timestamp', +new Date());
      (d.head || d.body).appendChild(s);
      })();
      </script>
      <noscript>Please enable JavaScript to view the <a href='https://disqus.com/?ref_noscript' rel='nofollow'>comments powered by Disqus.</a></noscript>"
    )
    ))