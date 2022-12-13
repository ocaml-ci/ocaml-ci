open Tyxml.Html
module Client = Ocaml_ci_api.Client
module Run_time = Ocaml_ci_client_lib.Run_time

module Make (M : Git_forge_intf.Forge) = struct
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
                  div ~a:[ a_id "step-status" ] [ Common.status_icon status ];
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
                        ~a:[ a_class [ "hidden md:block text-gray-500" ] ]
                        [
                          div
                            ~a:
                              [ a_class [ "hidden md:flex text-sm space-x-2" ] ]
                            [
                              div
                                ~a:[ a_id "step-created-at" ]
                                [ txt @@ Fmt.str "Created at %s" created_at ];
                              div
                                ~a:[ a_class [ "hidden md:inline" ] ]
                                [ txt "-" ];
                              div
                                ~a:[ a_id "step-queued-for" ]
                                [ txt @@ Fmt.str "%s in queue" queued_for ];
                              div
                                ~a:[ a_class [ "hidden md:inline" ] ]
                                [ txt "-" ];
                              div
                                ~a:[ a_id "step-finished-at" ]
                                [ txt @@ Fmt.str "Finished at %s" finished_at ];
                              div
                                ~a:[ a_class [ "hidden md:inline" ] ]
                                [ txt "-" ];
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
                ~a:
                  [
                    a_id "step-ran-for"; a_class [ "hidden md:inline text-sm" ];
                  ]
                [ txt @@ Fmt.str "Ran for %s" ran_for ];
              rebuild_button;
            ];
        ])

  let link_forge_commit ~org ~repo ~hash =
    a ~a:[ a_href (M.commit_url ~org ~repo ~hash) ] [ txt hash ]

  let link_forge_refs ~org ~repo refs =
    let refs = List.map M.parse_ref refs in
    let f = function
      | `Branch branch ->
          span
            ~a:[ a_class [ "flex flex-row items-center space-x-2" ] ]
            [
              span [ txt branch ];
              Common.build_history_button
                (Url.history_url M.prefix ~org ~repo
                   ~ref:(Printf.sprintf "branch/%s" branch));
              a
                ~a:[ a_href (M.branch_url ~org ~repo branch) ]
                [ Common.external_link ];
            ]
      | `Request id ->
          let id = string_of_int id in
          span
            ~a:[ a_class [ "flex flex-row items-center space-x-2" ] ]
            [
              span [ txt (M.request_abbrev ^ "#" ^ id) ];
              Common.build_history_button
                (Url.history_url M.prefix ~org ~repo
                   ~ref:(Printf.sprintf "%s/%s" M.request_prefix id));
              a
                ~a:
                  [
                    a_class [ "flex items-center space-x-2" ];
                    a_href (M.request_url ~org ~repo id);
                  ]
                [ Common.external_link ];
            ]
      | `Unknown _ -> txt ""
    in
    List.map f refs

  let list ~org ~repo ~message ~refs ~hash ~jobs ~first_step_queued_at
      ~total_run_time ~build_run_time ?(flash_messages = [])
      ?(build_status : Client.State.t = Passed) ~csrf_token () =
    let can_cancel =
      let check job_info =
        match job_info.Client.outcome with
        | Active | NotStarted -> true
        | Aborted | Failed _ | Passed | Undefined _ -> false
      in
      List.exists check jobs
    in
    let can_rebuild =
      let check job_info =
        match job_info.Client.outcome with
        | Active | NotStarted | Passed -> false
        | Aborted | Failed _ | Undefined _ -> true
      in
      List.exists check jobs
    in
    let show_rebuild = (not can_cancel) && can_rebuild in
    let buttons =
      Common.form_cancel ~hash ~csrf_token ~show:can_cancel ()
      :: Common.rebuild_button ~hash ~csrf_token ~show:show_rebuild ()
    in
    (* FIXME: Remove below when we are ready to show history for Gitlab as well. *)
    let title_card =
      Build.title_card ~status:build_status ~card_title:message
        ~hash_link:(link_forge_commit ~org ~repo ~hash:(Common.short_hash hash))
        ~ref_links:(link_forge_refs ~org ~repo refs)
        ~first_created_at:
          (Timestamps_durations.pp_timestamp first_step_queued_at)
        ~ran_for:(Timestamps_durations.pp_duration (Some build_run_time))
        ~total_run_time:(Timestamps_durations.pp_duration (Some total_run_time))
        ~buttons
    in
    let steps_table_div =
      div
        ~a:
          [
            a_class [ "bg-gray-50 px-6 py-3 text-gray-500 text-xs font-medium" ];
          ]
          (* TODO: We need to start with no stage separation - introduce Analysis/Checks and Build steps later *)
        [ txt "Build" ]
    in
    let steps_table =
      List.fold_left
        (fun l j ->
          let build_created_at =
            Option.value ~default:0. first_step_queued_at
          in
          let ts = Result.to_option @@ Run_time.timestamps_from_job_info j in
          let rt =
            Option.map (Run_time.run_times_from_timestamps ~build_created_at) ts
          in
          let created_at = Timestamps_durations.pp_timestamp j.queued_at in
          let queued_for =
            Timestamps_durations.pp_duration (Option.map Run_time.queued_for rt)
          in
          let ran_for =
            Timestamps_durations.pp_duration (Option.map Run_time.ran_for rt)
          in
          let step_uri = Url.job_url M.prefix ~org ~repo ~hash j.variant in
          List.append l
            [
              Build.step_row ~step_title:j.variant ~created_at ~queued_for
                ~ran_for ~status:j.outcome ~step_uri;
            ])
        [ steps_table_div ] jobs
    in
    Template_v1.instance
      [
        Tyxml.Html.script ~a:[ a_src "/js/build-page-poll.js" ] (txt "");
        Common.breadcrumbs
          [ (M.prefix, M.prefix); (org, org); (repo, repo) ]
          (Printf.sprintf "%s" (Common.short_hash hash));
        title_card;
        Common.flash_messages flash_messages;
        Build.tabulate_steps steps_table;
      ]

  let show ~org ~repo ~refs ~hash ~variant ~job ~status ~csrf_token ~timestamps
      ~build_created_at ~step_created_at ~step_finished_at ~can_rebuild
      ~can_cancel ?(flash_messages = []) (data, next) =
    ignore can_cancel;

    (* FIXME: Implement cancel step *)
    let header, footer =
      let button =
        Some
          (Common.form_rebuild_step ~variant ~csrf_token ~show:can_rebuild ())
      in
      let branch =
        if refs = [] then ""
        else
          match Astring.String.cuts ~sep:"/" (List.hd refs) with
          | "refs" :: "heads" :: branch -> Astring.String.concat ~sep:"/" branch
          | _ -> ""
      in
      let build_created_at = Option.value ~default:0. build_created_at in
      let run_time =
        Option.map
          (Run_time.run_times_from_timestamps ~build_created_at)
          timestamps
      in
      let title_card =
        title_card ~status ~card_title:variant
          ~hash_link:
            (link_forge_commit ~org ~repo ~hash:(Common.short_hash hash))
          ~created_at:(Timestamps_durations.pp_timestamp step_created_at)
          ~finished_at:(Timestamps_durations.pp_timestamp step_finished_at)
          ~queued_for:
            (Timestamps_durations.pp_duration
               (Option.map Run_time.queued_for run_time))
          ~ran_for:
            (Timestamps_durations.pp_duration
               (Option.map Run_time.ran_for run_time))
          ~button
      in
      let steps_to_reproduce_build =
        Tyxml.Html.(
          div
            ~a:
              [
                a_class
                  [
                    "shadow-sm rounded-lg overflow-hidden border \
                     border-gray-200 divide-x divide-gray-20";
                  ];
              ]
            [
              div
                ~a:
                  [
                    a_class
                      [ "flex items-center justify-between px-4 py-3 bg-white" ];
                  ]
                [
                  div
                    ~a:
                      [
                        Tyxml_helpers.at_click "stepsToRepro = !stepsToRepro";
                        a_class
                          [
                            "text-gray-900 text-base font-medium border-b-none \
                             border-gray-200 flex items-center space-x-3 \
                             flex-1 cursor-pointer";
                          ];
                      ]
                    [
                      Tyxml.Svg.(
                        Tyxml.Html.svg
                          ~a:
                            [
                              a_class [ "h-5 w-5 rotate-180" ];
                              Tyxml_helpers.a_svg_custom ":class"
                                "{ 'rotate-180': stepsToRepro == 1 }";
                              a_fill `None;
                              a_viewBox (0., 0., 24., 24.);
                              a_stroke `CurrentColor;
                              a_stroke_width (2., Some `Px);
                            ]
                          [
                            path
                              ~a:
                                [
                                  a_stroke_linecap `Round;
                                  a_stroke_linejoin `Round;
                                  a_d "M19 9l-7 7-7-7";
                                ]
                              [];
                          ]);
                      div [ txt "Steps to Reproduce" ];
                    ];
                  div
                    [
                      Tyxml.Html.button
                        ~a:
                          [
                            a_class [ "btn btn-sm btn-default" ];
                            Tyxml_helpers.at_click
                              "codeCopied = true, \
                               $clipboard($refs.reproCode.innerText)";
                          ]
                        [
                          Tyxml.Svg.(
                            Tyxml.Html.svg
                              ~a:
                                [
                                  a_class [ "h-5 w-5" ];
                                  a_fill `None;
                                  a_viewBox (0., 0., 24., 24.);
                                  a_stroke `CurrentColor;
                                  a_stroke_width (2., Some `Px);
                                ]
                              [
                                path
                                  ~a:
                                    [
                                      a_stroke_linecap `Round;
                                      a_stroke_linejoin `Round;
                                      a_d
                                        "M8 16H6a2 2 0 01-2-2V6a2 2 0 \
                                         012-2h8a2 2 0 012 2v2m-6 12h8a2 2 0 \
                                         002-2v-8a2 2 0 00-2-2h-8a2 2 0 00-2 \
                                         2v8a2 2 0 002 2z";
                                    ]
                                  [];
                              ]);
                          txt "Copy code";
                        ];
                    ];
                ];
              div
                ~a:
                  [
                    a_class
                      [
                        "flex relative overflow-hidden transition-all max-h-0 \
                         duration-700";
                      ];
                    Tyxml_helpers.x_ref "container1";
                    Tyxml_helpers.x_bind_style
                      "stepsToRepro == 1 ? 'max-height: ' + \
                       $refs.container1.scrollHeight + 'px' : ''";
                  ]
                [
                  div
                    ~a:
                      [
                        a_class
                          [
                            "bg-gray-900 px-6 py-3 rounded-lg rounded-l-none \
                             text-gray-300 w-full rounded-t-none";
                          ];
                      ]
                    [
                      Tyxml.Html.code
                        ~a:
                          [
                            a_id "build-repro";
                            a_class [ "overflow-auto" ];
                            Tyxml_helpers.x_ref "reproCode";
                          ]
                        [];
                    ];
                ];
            ])
      in
      let logs_container =
        Tyxml.Html.(
          div
            ~a:
              [
                a_class [ "mt-6 bg-gray-100 rounded-lg relative border" ];
                Tyxml_helpers.x_data "codeLink";
                Tyxml_helpers.x_init "highlightLine";
              ]
            [
              Tyxml.Html.button
                ~a:
                  [
                    a_class [ "copy-link-btn" ];
                    Tyxml_helpers.at_click "copyCode";
                    Tyxml_helpers.x_show "manualSelection";
                    Tyxml_helpers.x_ref "copyLinkBtn";
                    Tyxml_helpers.x_cloak;
                  ]
                [
                  Tyxml.Svg.(
                    Tyxml.Html.svg
                      ~a:
                        [
                          a_class [ "w-4 h-4" ];
                          a_fill `None;
                          a_viewBox (0., 0., 24., 24.);
                          a_stroke_width (2., Some `Px);
                          a_stroke `CurrentColor;
                        ]
                      [
                        path
                          ~a:
                            [
                              a_stroke_linecap `Round;
                              a_stroke_linejoin `Round;
                              a_d
                                "M13.19 8.688a4.5 4.5 0 011.242 7.244l-4.5 \
                                 4.5a4.5 4.5 0 \
                                 01-6.364-6.364l1.757-1.757m13.35-.622l1.757-1.757a4.5 \
                                 4.5 0 00-6.364-6.364l-4.5 4.5a4.5 4.5 0 \
                                 001.242 7.244";
                            ]
                          [];
                      ]);
                ];
              div
                ~a:[ a_class [ "table-overflow overflow-auto rounded-lg" ] ]
                [ txt "@@@" ];
            ])
      in
      let link_copied_notification =
        Tyxml.Html.(
          div
            ~a:
              [
                a_class [ "notification" ];
                Tyxml_helpers.x_cloak;
                Tyxml_helpers.x_show "linkCopied";
                Tyxml_helpers.x_transition;
              ]
            [
              div
                ~a:[ a_class [ "flex items-center space-x-2" ] ]
                [
                  div
                    ~a:[ a_class [ "icon-status icon-status--success" ] ]
                    [
                      Tyxml.Svg.(
                        Tyxml.Html.svg
                          ~a:
                            [
                              a_class [ "h-4 w-4" ];
                              a_viewBox (0., 0., 20., 20.);
                              a_fill (`Color ("#12B76A", None));
                            ]
                          [
                            path
                              ~a:
                                [
                                  Tyxml_helpers.a_svg_custom "fill-rule"
                                    "evenodd";
                                  Tyxml_helpers.a_svg_custom "clip-rule"
                                    "evenodd";
                                  a_d
                                    "M16.707 5.293a1 1 0 010 1.414l-8 8a1 1 0 \
                                     01-1.414 0l-4-4a1 1 0 011.414-1.414L8 \
                                     12.586l7.293-7.293a1 1 0 011.414 0z";
                                ]
                              [];
                          ]);
                    ];
                  div [ txt "Link copied" ];
                ];
              Tyxml.Html.button
                ~a:
                  [
                    a_class [ "icon-button" ];
                    Tyxml_helpers.at_click "linkCopied=false";
                  ]
                [
                  Tyxml.Svg.(
                    Tyxml.Html.svg
                      ~a:
                        [
                          a_fill `None;
                          a_viewBox (0., 0., 24., 24.);
                          a_stroke_width (2.5, Some `Px);
                          a_stroke `CurrentColor;
                          a_class [ "w-4 h-4" ];
                        ]
                      [
                        path
                          ~a:
                            [
                              a_stroke_linecap `Round;
                              a_stroke_linejoin `Round;
                              a_d "M4.5 19.5l15-15m-15 0l15 15";
                            ]
                          [];
                      ]);
                ];
            ])
      in
      let body =
        Template_v1.instance ~full:true
          [
            Tyxml.Html.script ~a:[ a_src "/js/log-highlight.js" ] (txt "");
            Tyxml.Html.script ~a:[ a_src "/js/step-page-poll.js" ] (txt "");
            Common.breadcrumbs
              [
                (M.prefix, M.prefix);
                (org, org);
                (repo, repo);
                ( Printf.sprintf "%s (%s)" (Common.short_hash hash) branch,
                  Printf.sprintf "commit/%s" hash );
              ]
              variant;
            title_card;
            Common.flash_messages flash_messages;
            div
              ~a:
                [
                  a_class [ "mt-8 flex flex-col" ];
                  Tyxml_helpers.x_data
                    "{ url: window.location.href, logs: true, artefacts: \
                     false, codeCoverage: false, codeCopied: false, \
                     linkCopied: false, startingLine: null, endingLine: null, \
                     manualSelection: false}";
                ]
              [
                link_copied_notification;
                div
                  ~a:
                    [
                      a_class
                        [
                          "flex space-x-6 border-b border-gray-200 mb-6 text-sm";
                        ];
                    ]
                  [ h3 ~a:[ a_class [ "font-medium pb-2" ] ] [ txt "Logs" ] ];
                div
                  ~a:
                    [
                      Tyxml_helpers.x_show "logs";
                      Tyxml_helpers.x_data "{stepsToRepro: true}";
                    ]
                  [ steps_to_reproduce_build; logs_container ];
              ];
            Tyxml.Html.script ~a:[ a_src "/js/add-repro-steps.js" ] (txt "");
          ]
      in
      Astring.String.cut ~sep:"@@@" body |> Option.get
    in
    let ansi = Ansi.create () in
    let line_number = ref 0 in
    let last_line_blank = ref false in
    let tabulate data : string =
      let aux (l : string) log_line =
        if !last_line_blank && log_line = "" then
          (* Squash consecutive new lines *)
          l
        else
          let is_start_of_steps_to_reproduce =
            Astring.String.is_infix ~affix:"To reproduce locally:" log_line
          in
          let is_end_of_steps_to_reproduce =
            Astring.String.is_infix ~affix:"END-REPRO-BLOCK" log_line
          in
          let code_line_class =
            if is_start_of_steps_to_reproduce then
              "code-line__code repro-block-start"
            else if is_end_of_steps_to_reproduce then
              "code-line__code repro-block-end"
            else "code-line__code"
          in
          last_line_blank := log_line = "";
          line_number := !line_number + 1;
          Printf.sprintf "%s\n%s" l
            (Fmt.str "%a" (pp_elt ())
               (tr
                  ~a:
                    [
                      a_class [ "code-line" ];
                      Tyxml_helpers.colon_class
                        "parseInt($el.id.substring(1, $el.id.length)) >= \
                         startingLine && parseInt($el.id.substring(1, \
                         $el.id.length)) <= endingLine ? 'highlight' : ''";
                      Tyxml_helpers.at_click "highlightLine";
                      a_id (Printf.sprintf "L%d" !line_number);
                    ]
                  [
                    td
                      ~a:[ a_class [ "code-line__number" ] ]
                      [ txt (Printf.sprintf "%d" !line_number) ];
                    td
                      ~a:[ a_class [ code_line_class ] ]
                      [ pre [ Unsafe.data log_line ] ];
                  ]))
      in
      Printf.sprintf "%s%s"
        (List.fold_left aux "<table id='steps-table'><tbody>" data)
        "</tbody></table>"
    in
    let open Lwt.Infix in
    Dream.stream
      ~headers:[ ("Content-type", "text/html; charset=utf-8") ]
      (fun response_stream ->
        Dream.write response_stream header >>= fun () ->
        let data' =
          data |> Ansi.process ansi |> Astring.String.cuts ~sep:"\n" |> tabulate
        in
        Dream.write response_stream data' >>= fun () ->
        let rec loop next =
          Current_rpc.Job.log job ~start:next >>= function
          | Ok ("", _) ->
              Dream.write response_stream footer >>= fun () ->
              Dream.close response_stream
          | Ok (data, next) ->
              Dream.log "Fetching logs";
              let data' =
                data
                |> Ansi.process ansi
                |> Astring.String.cuts ~sep:"\n"
                |> tabulate
              in
              Dream.write response_stream data' >>= fun () ->
              Dream.flush response_stream >>= fun () -> loop next
          | Error (`Capnp ex) ->
              Dream.log "Error fetching logs: %a" Capnp_rpc.Error.pp ex;
              Dream.write response_stream
                (Fmt.str "ocaml-ci error: %a@." Capnp_rpc.Error.pp ex)
        in
        loop next)
end
