library(wordVectors)
out <- uniword2vec::train_word2vec("../../BLM_project/the_nation_traindata.txt",
                                    output_file_in = "vectors_in.bin",
                                    output_file_out = "vectors_out.bin",
                                    cbow = 0, threads = 10,
                                    force = TRUE, min_count = 5,
                                    window = 10)

nearest_to(out$input, out$input[["black"]], 20)
nearest_to(out$output, out$output[["black"]], 20)

nearest_to(out$input, out$input[["police"]], 20)
nearest_to(out$output, out$output[["police"]], 20)

nearest_to(out$input, out$input[["lives"]], 20)
nearest_to(out$output, out$output[["lives"]], 20)

cosineSimilarity(out$input[["lives"]], out$input[["matter"]])

nearest_to(out$input, out$input[["mcdonald"]], 20)
nearest_to(out$output, out$output[["mcdonald"]], 20)

nearest_to(out$input, out$input[["racial"]], 20)
nearest_to(out$output, out$output[["racial"]], 20)

