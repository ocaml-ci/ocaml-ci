let title_card ~status ~card_title ~hash_link ~created_at ~finished_at
    ~queued_for ~ran_for ~button =
  let rebuild_button = Option.value ~default:(Tyxml.Html.div []) button in
  Tyxml.Html.(
    div
      ~a:[ a_class [ "justify-between items-center flex" ] ]
      [
        div
          ~a:[ a_class [ "flex flex-col space-y-6" ] ]
          [
            div
              ~a:[ a_class [ "flex items-center space-x-4" ] ]
              [
                Common.status_icon status;
                div
                  ~a:[ a_class [ "flex flex-col space-y-1" ] ]
                  [
                    div
                      ~a:[ a_class [ "flex items-baseline space-x-2" ] ]
                      [
                        h1 ~a:[ a_class [ "text-xl" ] ] [ txt card_title ];
                        (* TODO: Breakdown by OS, Compiler and Opam
                           <div class="text-sm font-normal text-gray-500">
                             OS: debian-11 - Compiler: 4.14+flambda - Opam: 2.1
                           </div>
                        *)
                      ];
                    div
                      ~a:[ a_class [ "text-gray-500" ] ]
                      [
                        div
                          ~a:[ a_class [ "flex text-sm space-x-2" ] ]
                          [
                            div [ txt @@ Fmt.str "Created at: %s" created_at ];
                            div [ txt "-" ];
                            div [ txt @@ Fmt.str "%s in queue" queued_for ];
                            div [ txt "-" ];
                            div [ txt @@ Fmt.str "Finished at: %s" finished_at ];
                            div [ txt "-" ];
                            div [ hash_link ];
                          ];
                      ];
                  ];
              ];
          ];
        div
          ~a:[ a_class [ "flex items-center justify-between space-x-4" ] ]
          [
            div
              ~a:[ a_class [ "text-sm" ] ]
              [ txt @@ Fmt.str "Ran for %s" ran_for ];
            rebuild_button;
          ];
      ])

let log_highlight_js =
  Tyxml.Html.script ~a:[]
    (Tyxml.Html.Unsafe.data
       {|
document.addEventListener('alpine:init', () => {
    Alpine.data('codeLink', () => ({
        permalinkButton: false,

        copyCode(e) {
          this.linkCopied = true;
          const index = this.url.indexOf("#");

          if (index >= 0) {
            this.url = this.url.substring(0, index);
          }

          if (this.endingLine) {
            this.url += `#L${this.startingLine}-${this.endingLine}`;
          } else {
            this.url += `#L${this.startingLine}`;
          }

          location.href = this.url;
          navigator.clipboard.writeText(this.url);
          // this.$clipboard(this.url);
          this.manualSelection = false;
        },

        positionCopyButton(e) {
            this.$refs.copyLinkBtn.style.top = `${e.layerY-15}px`;
        },

        highlightLine(e) {
            if (e) {
              const currentLine = e.target.parentNode.id;
              const currentID = parseInt(currentLine.substring(1, currentLine.length));
              this.manualSelection = true;
              this.positionCopyButton(e);

              if (!this.startingLine) {
                  this.startingLine = currentID;
                  this.endingLine = currentID;
                  console.log(this.startingLine);
              }

              if (this.startingLine) {
                  if (e.shiftKey) {
                      if (currentID > this.startingLine) {
                          this.endingLine = currentID;
                      } else if (currentID < this.startingLine) {
                          this.endingLine = this.startingLine;
                          this.startingLine = currentID;
                      } else {
                          this.startingLine = currentID;
                          this.endingLine = currentID;
                      }
                  } else {
                      this.startingLine = currentID;
                      this.endingLine = currentID;
                      this.linkCopied = false;
                  }
              }
            } else {
              const index = this.url.indexOf("#")+2;

              if (index >= 0) {
                const lines = this.url.substring(index, this.url.length);
                const lineNumbers = lines.split("-");
                this.startingLine = parseInt(lineNumbers[0]);
                this.endingLine = parseInt(lineNumbers[1]);
              }

              if (this.startingLine) {
                setTimeout(() => {
                  console.log(this.startingLine);
                  document.getElementById(`L${this.startingLine}`).scrollIntoView();
                }, 500)
              }
            }
        }
    }))
})
|})
