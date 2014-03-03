Cleanpowerperks::Application.routes.draw do

  root :to => "home#index"

  resources :perks
end
