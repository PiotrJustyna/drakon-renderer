for file in ./app/*.hs; do
  echo "formatting $file..."
  mv "$file" "${file%.hs}_preformatted.hs"
  cat "${file%.hs}_preformatted.hs" | hindent --line-length 80 --indent-size 2 --sort-imports > "$file"
  rm "${file%.hs}_preformatted.hs"
  echo "...done"
done
