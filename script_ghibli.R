
# bibliotecas -------------------------------------------------------------

#web scraping
library(httr)
library(xml2)
library(rvest)
library(progressr)
library(beepr)
#dados
library(dplyr)
library(purrr)
library(janitor)
library(tidyr)
library(stringr)
#graficos
library(ggplot2)
library(ghibli)
library(ggimage)
#tabela
library(gt)


# scraping simples -----------------------------------------------------------

# url

u_ghibli <- "https://pt.wikipedia.org/wiki/Studio_Ghibli"

# requisicao

r_ghibli <- httr::GET(
  u_ghibli, 
  httr::write_disk("output/filmes_ghibli.html", overwrite = TRUE) # \salvar pagina no disco
)

# coletando tabela

tabela_ghibli <- r_ghibli |> 
  xml2::read_html() |> #ler arquivo HTML
  xml2::xml_find_first("//*[@id='mw-content-text']/div[1]/table[3]") |> #parametro XPath da tabela
  rvest::html_table(header = TRUE) |> #parsing #transformar dados HTML em tibble
  clean_names() |> 
  rename(roteirista = roteirista_s, produtor = produtor_es)

# scraping complexa ----------------------------------------------------------

links_filmes <- r_ghibli |> 
  xml2::read_html() |> # ler arquivo HTML
  xml2::xml_find_all("//table[@class='wikitable unsortable']//a") |> #coletar nos da tabela com a tag <a>
  xml2::xml_attr("href") #coletar atributos href

# criando links validos

links_filmes <- paste0("https://pt.wikipedia.org", links_filmes)

# funcao para obter requisicoes

get_links <- function(link, id) {
  arquivo <- paste0("output/", id, ".html")
  r <- httr::GET(link, write_disk(arquivo, overwrite = TRUE))
  arquivo
}

# funcao para obter as requisicoes de uma unica vez, com identificacao de erro

get_progresso <- function(link, id, progresso) {
  progresso() #progresso
  get_erro <- purrr::possibly(get_links, otherwise = "ERRO NA REQUISIÇÃO") #mapear erros nas requisicoes
  get_erro(link, id)
}

# obtendo requisicoes com erro, beep e barra de progresso

progressr::with_progress({
  # unindo handlers de progresso e beep
  progressr::handlers(
    list(
      progressr::handler_progress(),
      progressr::handler_beepr(1L, 2L, 3L)
    )
  )
  # barra de progresso
  progresso <- progressr::progressor(length(links_filmes))
  purrr::map2(
    links_filmes, seq_along(links_filmes), 
    get_progresso, p = progresso
  )
})


# primeira analise --------------------------------------------------------

# manipulacao

tabela_ghibli <- tabela_ghibli |> 
  mutate(
    roteirista = recode(
      roteirista, 
      `Goro MiyazakiKeiko Niwa` = "Goro Miyazaki, Keiko Niwa",
      `Hayao MiyazakiKeiko Niwa` = "Hayao Miyazaki, Keiko Niwa",
      `Masashi AndoKeiko NiwaHiromasa Yonebayashi` = "Masashi Ando, Keiko Niwa, Hiromasa Yonebayashi",
      `Keiko NiwaEmi Gunji` = "Keiko Niwa, Emi Gunji"
    ),
    produtor = recode(
      produtor, 
      `Takashi ShojiSeiichiro Ujiie` = "Takashi Shoji, Seiichiro Ujiie",
      `Toshio SuzukiNozomu Takahashi` = "Toshio Suzuki, Nozomu Takahashi",
      `Toshio SuzukiTomohiko Ishii` = "Toshio Suzuki, Tomohiko Ishii",
      `Yoshiaki NishimuraSeiichiro Ujiie` = "Yoshiaki Nishimura, Seiichiro Ujiie",
      `Hayao MiyazakiToshio Suzuki` = "Hayao Miyazaki, Toshio Suzuki"
    )
  )

# grafico animado

ghibli <- "ghibli.png" #imagem do totoro

tabela_ghibli |> 
  filter(ano != "ASA") |> 
  mutate(
    ano = as.numeric(ano),
    titulo = forcats::fct_reorder(titulo, ano)
  ) |> 
  ggplot(aes(x = ano, y = titulo)) +
  scale_x_continuous(limits = c(1986, 2020), breaks = seq(1986, 2020, by = 4)) +
  geom_image(image = ghibli, size = .04) + #adiciona imagem
  theme_classic() +
  theme_light() +
  labs(
    title = "Ano de lançamento dos filmes do Studio Ghibli",
    x = "", y = "Filme"
  )

# grafico de barras

tabela_ghibli |> 
  group_by(diretor) |> 
  summarise(n = n()) |> 
  mutate(diretor = forcats::fct_reorder(diretor, -n)) |> 
  ggplot(aes(x = diretor, y = n, fill = diretor)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(breaks = seq(0, 10, by = 2)) +
  theme_classic() +
  theme_light() +
  scale_fill_ghibli_d("MononokeMedium") + #paleta de cor ghibli: 'princesa mononoke'
  geom_label(
    aes(label = n), position = position_dodge(width = 1), 
    show.legend = FALSE, color = "white"
  ) +
  labs(
    title = "Número de filmes do Studio Ghibli, por diretor", 
    x = "", y = "", 
    color = "diretor", fill = "Diretor:"
  ) +
  theme(
    legend.position = "bottom",
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  )


# segunda analise ---------------------------------------------------------

# manipulacao

tumulo_vagalumes <- xml2::read_html("output/5.html") |> 
  rvest::html_table() |> #parsing
  pluck(1) %>% 
  .[c(-1, -2), ]

colnames(tumulo_vagalumes) <- c("Categoria", "Descrição")

tumulo_vagalumes <- tumulo_vagalumes |> 
  pivot_wider(names_from = "Categoria", values_from = "Descrição") |>
  clean_names() |> 
  rename(
    titulo_br = no_brasil,
    titulo_pt = em_portugal,
    duracao_min = japao1988_cor_89_min,
    produtora = companhia_s_produtora_s,
    receita_milhoes = receita
  ) |> 
  mutate(
    titulo_br = str_sub(titulo_br, end = 22),
    titulo_pt = str_sub(titulo_pt, end = 23),
    duracao_min = str_sub(duracao_min, start = 21, end = 22),
    elenco = recode(elenco, `Tsutomu TatsumiAyano ShiraishiYoshiko ShinoharaAkemi Yamaguchi` = "Tsutomu Tatsumi, Ayano Shiraishi, Yoshiko Shinohara, Akemi Yamaguchi"),
    genero = recode(genero, `drama[5]guerra[6]` = "drama, guerra"),
    lancamento = str_sub(lancamento, start = 22, end = 31),
    receita_milhoes = str_sub(receita_milhoes, start = 2, end = 5)
  )

# tabela 

tumulo_vagalumes |> 
  pivot_longer(
    cols = 1:17, 
    names_to = "Categoria", values_to = "Descrição"
  ) |> 
  gt() |> 
  tab_header(
    title = "Túmulo de Vagalumes", 
    subtitle = "Ficha do filme"
  ) |> 
  gtsave("tab_ghibli.png")

