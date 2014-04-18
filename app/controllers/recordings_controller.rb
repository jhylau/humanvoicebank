class RecordingsController < ApplicationController
    before_filter :authenticate_user!, except: [:index, :show]
    
	def index
	end
end