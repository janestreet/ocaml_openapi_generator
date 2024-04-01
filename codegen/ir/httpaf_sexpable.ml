open! Core

module Status = struct
  include Httpaf.Status
  include Sexpable.Of_stringable (Httpaf.Status)
end

module Method = struct
  include Httpaf.Method
  include Sexpable.Of_stringable (Httpaf.Method)
end
