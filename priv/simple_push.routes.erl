#{prefix => "",
  security => {simple_push_auth, basic_auth},
  routes => [
             {"/push/apns", { simple_push_account_controller, apns}, #{methods => [post]}},
             {"/push/:push/send", { simple_push_send_controller, send}, #{methods => [post]}}
            ],
  statics => [
              {"/assets/[...]", "assets"}
             ]
 }.
