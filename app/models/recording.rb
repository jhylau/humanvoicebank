class Recording < ActiveRecord::Base
  # Include default devise modules. Others available are:
  # :confirmable, :lockable, :timeoutable and :omniauthable
  mount_uploader :audio, AudioUploader

  belongs_to :user
  belongs_to :sentence
end
