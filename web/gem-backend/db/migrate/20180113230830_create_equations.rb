class CreateEquations < ActiveRecord::Migration[5.1]
  def change
    create_table :equations do |t|
      t.string :name
      t.jsonb :content

      t.timestamps
    end
  end
end
