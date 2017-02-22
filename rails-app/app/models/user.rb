class User < ApplicationRecord
    has_many :events, -> { order(felt_at: :desc) }
end
