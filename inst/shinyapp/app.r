# 1. 安装必要包
# install.packages(c("shiny", "shinydashboard", "DT", "plotly", "shinyjs", "mzR"))

# 2. 加载所需库
library(shiny)
library(DT)
library(plotly)
library(shinyjs)
library(mzR)
library(dplyr)
library(tidyr)
library(readr)

# ==================== UI部分 ====================
ui <- fluidPage(
  # 自定义CSS样式
  tags$head(
    tags$style(HTML("
      body {
        font-family: 'Segoe UI', Arial, sans-serif;
        font-size: 13px;
        padding: 10px;
        background-color: #f5f7fa;
      }
      .main-header {
        text-align: center;
        margin-bottom: 20px;
        padding-bottom: 15px;
        border-bottom: 2px solid #2c3e50;
      }
      .upload-section {
        background-color: white;
        padding: 20px;
        border-radius: 8px;
        border: 1px solid #dee2e6;
        margin-bottom: 20px;
        box-shadow: 0 2px 4px rgba(0,0,0,0.05);
      }
      .result-section {
        background-color: white;
        padding: 20px;
        border-radius: 8px;
        border: 1px solid #dee2e6;
        box-shadow: 0 2px 4px rgba(0,0,0,0.05);
      }
      .btn-success {
        background: linear-gradient(to right, #28a745, #20c997);
        border: none;
        width: 100%;
        padding: 12px;
        font-size: 14px;
        font-weight: bold;
        border-radius: 5px;
        transition: all 0.3s;
      }
      .btn-success:hover {
        transform: translateY(-2px);
        box-shadow: 0 4px 8px rgba(40, 167, 69, 0.3);
      }
      .btn-primary {
        background: linear-gradient(to right, #007bff, #0056b3);
        border: none;
        border-radius: 5px;
        padding: 8px 15px;
        transition: all 0.3s;
      }
      .btn-primary:hover {
        transform: translateY(-2px);
        box-shadow: 0 4px 8px rgba(0, 123, 255, 0.3);
      }
      .btn-warning {
        background: linear-gradient(to right, #ffc107, #fd7e14);
        border: none;
        border-radius: 5px;
        padding: 8px 15px;
        transition: all 0.3s;
      }
      .btn-warning:hover {
        transform: translateY(-2px);
        box-shadow: 0 4px 8px rgba(255, 193, 7, 0.3);
      }
      .progress-container {
        position: fixed;
        bottom: 20px;
        right: 20px;
        width: 320px;
        background-color: white;
        border: 1px solid #dee2e6;
        border-radius: 8px;
        padding: 15px;
        box-shadow: 0 4px 12px rgba(0,0,0,0.15);
        z-index: 1000;
      }
      .progress-label {
        font-size: 13px;
        margin-bottom: 8px;
        color: #2c3e50;
      }
      .plot-container {
        height: 400px;
        margin-bottom: 20px;
        border: 1px solid #eee;
        border-radius: 5px;
        padding: 10px;
        background-color: white;
      }
      h4 {
        color: #2c3e50;
        margin-bottom: 15px;
        padding-bottom: 10px;
        border-bottom: 2px solid #3498db;
        font-weight: 600;
      }
      .file-info {
        background-color: #e8f4fd;
        padding: 10px;
        border-radius: 5px;
        border-left: 4px solid #3498db;
        margin-top: 10px;
        font-size: 12px;
        color: #2c3e50;
      }
      .modal-dialog {
        max-width: 95%;
        width: 95%;
      }
      .modal-content {
        border-radius: 10px;
        border: none;
        box-shadow: 0 10px 30px rgba(0,0,0,0.2);
      }
      .modal-header {
        background: linear-gradient(to right, #2c3e50, #4a6491);
        color: white;
        border-radius: 10px 10px 0 0;
        border-bottom: none;
        padding: 15px 20px;
      }
      .modal-header .close {
        color: white;
        text-shadow: none;
        opacity: 0.8;
        font-size: 24px;
      }
      .modal-header .close:hover {
        opacity: 1;
      }
      .modal-body {
        padding: 20px;
        background-color: #f8f9fa;
      }
      .parameter-box {
        background-color: #f8f9fa;
        padding: 15px;
        border-radius: 5px;
        border: 1px solid #dee2e6;
        margin-bottom: 10px;
      }
      .nav-tabs {
        border-bottom: 2px solid #dee2e6;
        margin-bottom: 15px;
      }
      .nav-tabs .nav-link.active {
        background-color: #007bff;
        color: white;
        border-color: #007bff;
        font-weight: bold;
      }
      .pfas-fragment {
        background-color: #fff3cd;
        border: 1px solid #ffeaa7;
        padding: 3px 6px;
        border-radius: 3px;
        margin: 2px;
        display: inline-block;
        font-size: 11px;
      }
      .pfas-high {
        background-color: #d4edda;
        border-color: #c3e6cb;
      }
      .pfas-medium {
        background-color: #fff3cd;
        border-color: #ffeaa7;
      }
    "))
  ),
  
  useShinyjs(),
  
  # 主标题
  div(
    class = "main-header",
    h3("Build ms2 database v1.0", style = "color: #2c3e50; font-weight: 700;"),
  ),
  
  # 上传和处理区域
  div(
    class = "upload-section",
    fluidRow(
      column(
        width = 5,
        h4("📁 文件上传"),
        
        fileInput(
          "compound_file",
          "上传化合物CSV文件",
          accept = c(".csv", ".txt"),
          buttonLabel = "浏览...",
          placeholder = "选择CSV文件",
          width = "100%"
        ),
        
        fileInput(
          "ms2_file",
          "上传质谱数据文件",
          accept = c(".mzML", ".mzXML"),
          buttonLabel = "浏览...",
          placeholder = "选择mzML/mzXML文件",
          width = "100%"
        ),
        
        uiOutput("file_info_ui"),
        
        hr(),
        
        h4("⚙️ 处理控制"),
        
        actionButton("start_processing", "开始构建数据库", 
                     icon = icon("database"), 
                     class = "btn-success",
                     style = "margin-top: 10px;")
      ),
      
      column(
        width = 7,
        h4("🔧 参数设置"),
        
        tabsetPanel(
          tabPanel(
            "基本参数",
            div(
              class = "parameter-box",
              fluidRow(
                column(6,
                       numericInput("mz_tol", "m/z容差 (Da)", 
                                    value = 0.01, min = 0.001, max = 0.1, step = 0.001,
                                    width = "100%")
                ),
                column(6,
                       numericInput("ppm", "ppm容差", 
                                    value = 20, min = 5, max = 100, step = 5,
                                    width = "100%")
                )
              ),
              
              fluidRow(
                column(6,
                       numericInput("rt_tol", "RT容差 (秒)", 
                                    value = 6, min = 5, max = 300, step = 5,
                                    width = "100%")
                ),
                column(6,
                       numericInput("min_intensity", "最小强度", 
                                    value = 100, min = 1, max = 1000, step = 10,
                                    width = "100%")
                )
              )
            )
          ),
          
          tabPanel(
            "EIC参数",
            div(
              class = "parameter-box",
              fluidRow(
                column(6,
                       numericInput("eic_mz_tol", "EIC m/z容差", 
                                    value = 0.01, min = 0.001, max = 0.1, step = 0.001,
                                    width = "100%")
                ),
                column(6,
                       numericInput("eic_rt_window", "EIC RT窗口 (秒)", 
                                    value = 30, min = 5, max = 300, step = 5,
                                    width = "100%")
                )
              )
            )
          ),
          
          tabPanel(
            "PFAS碎片检测",
            div(
              class = "parameter-box",
              h5("PFAS特征碎片检测设置"),
              br(),
              fluidRow(
                column(6,
                       checkboxInput("enable_pfas_detection", "启用PFAS碎片检测", 
                                    value = FALSE, width = "100%")
                ),
                column(6,
                       numericInput("pfas_mz_tol", "PFAS碎片m/z容差 (Da)", 
                                   value = 0.01, min = 0.001, max = 0.1, step = 0.001,
                                   width = "100%")
                )
              ),
              fluidRow(
                column(12,
                       div(
                         style = "margin-top: 15px;",
                         h6("特征碎片列表预览:"),
                         div(
                           style = "max-height: 200px; overflow-y: auto; border: 1px solid #dee2e6; padding: 10px; border-radius: 5px; background-color: #f8f9fa;",
                           verbatimTextOutput("pfas_fragments_preview")
                         )
                       )
                )
              )
            )
          )
        )
      )
    )
  ),
  
  # 结果展示区域
  div(
    class = "result-section",
    h4("📊 数据库结果"),
    
    fluidRow(
      column(
        width = 12,
        div(
          style = "margin-bottom: 15px; text-align: center;",
          downloadButton("download_csv", "下载CSV", class = "btn-primary", 
                         style = "margin-right: 5px;"),
          downloadButton("download_rds", "下载RDS", class = "btn-primary", 
                         style = "margin-right: 5px;"),
          downloadButton("download_msp", "下载MSP", class = "btn-primary",
                         style = "margin-right: 5px;"),
          downloadButton("download_pfas_report", "下载PFAS报告", class = "btn-warning")
        ),
        
        DTOutput("database_table")
      )
    ),
    
    hr(),
    
    h4("📈 PFAS检测统计"),
    
    fluidRow(
      column(6, plotlyOutput("pfas_detection_plot", height = "300px")),
      column(6, plotlyOutput("pfas_class_plot", height = "300px"))
    )
  ),
  
  # 右下角进度条
  uiOutput("progress_ui"),
  
  # 可视化弹窗 - 使用bootstrap modal
  tags$div(
    id = "visualizationModal",
    class = "modal fade",
    tabindex = "-1",
    role = "dialog",
    `aria-labelledby` = "visualizationModalLabel",
    `aria-hidden` = "true",
    tags$div(
      class = "modal-dialog modal-xl",
      role = "document",
      tags$div(
        class = "modal-content",
        tags$div(
          class = "modal-header",
          tags$h4(class = "modal-title", id = "visualizationModalLabel", 
                  textOutput("modal_title")),
          tags$button(
            type = "button",
            class = "close",
            `data-dismiss` = "modal",
            `aria-label` = "Close",
            onclick = "$('#visualizationModal').modal('hide');",
            tags$span(`aria-hidden` = "true", "×")
          )
        ),
        tags$div(
          class = "modal-body",
          fluidRow(
            column(
              width = 12,
              div(
                class = "plot-container",
                plotlyOutput("modal_eic_plot", height = "350px")
              )
            )
          ),
          fluidRow(
            column(
              width = 12,
              div(
                class = "plot-container",
                plotlyOutput("modal_ms2_plot", height = "350px")
              )
            )
          ),
          fluidRow(
            column(
              width = 12,
              style = "text-align: center; margin-top: 20px;",
              downloadButton("download_combined", "下载谱图数据", 
                             class = "btn-primary",
                             style = "padding: 10px 20px; font-size: 14px;")
            )
          )
        )
      )
    )
  ),
  
  # 初始化模态框的JavaScript
  tags$script(HTML("
    // 初始化模态框
    $(document).ready(function() {
      $('#visualizationModal').modal({show: false, backdrop: 'static'});
      
      // 监听关闭按钮点击
      $('#visualizationModal .close').click(function() {
        $('#visualizationModal').modal('hide');
      });
      
      // 监听模态框外部点击关闭
      $('#visualizationModal').on('click', function(e) {
        if (e.target === this) {
          $('#visualizationModal').modal('hide');
        }
      });
    });
    
    // 显示模态框的函数
    function showVisualizationModal() {
      $('#visualizationModal').modal('show');
    }
  "))
)

# ==================== 服务器部分 ====================
server <- function(input, output, session) {
  options(shiny.maxRequestSize = 30000 * 1024 ^ 2)
  
  # ==================== 响应式值 ====================
  values <- reactiveValues(
    compounds = NULL,
    database = NULL,
    processing = FALSE,
    progress_value = 0,
    progress_text = "等待开始",
    selected_spec = NULL
  )
  
  # ==================== PFAS特征碎片数据 ====================
  pfas_fragments <- reactive({
    data.frame(
      m_z = c(
        63.96245, 77.96552, 79.95736, 82.96085, 84.99067, 98.95577,
        118.99256, 168.98937, 184.98429, 218.98618, 412.96643,
        497.9462, 498.93022, 583.98298, 62.98878, 63.96245,
        64.97027, 68.99576, 77.96552, 79.95736, 80.99576, 82.96085,
        84.99067, 91.98117, 98.95577, 118.99256, 121.99174, 130.99256,
        134.98748, 136.00739, 146.98748, 162.98239, 166.01795, 168.98937,
        180.98937, 184.98429, 196.98429, 218.98618, 230.98618, 234.98109,
        268.98298, 280.98298, 301.98446, 318.97979, 418.9734
      ),
      formula = c(
        "O2S", "NO2S", "O3S", "FO2S", "CF3O", "FO3S", "C2F5", "C3F7",
        "C3F7O", "C4F9", "C8F15O2", "C8HF17NO2S", "C8F17O3S", "C12H7F17NO4S",
        "CO2F", "SO2", "SO2H", "CF3", "O2SN", "SO3", "C2F3", "SO2F", "CF3O",
        "O2SNCH2", "SO3F", "C2F5", "O2SNC2H4O", "C3F5", "C2F5O", "O2SNC3H6O",
        "C3F5O", "C3F5O2", "O2SNC4H8O2", "C3F7", "C4F7", "C3F7O", "C4F7O",
        "C4F9", "C5F9", "C4F9O", "C5F11", "C6F11", "C5F12N", "C6F13", "C8F17"
      ),
      compound_type = c(
        "Sulfonate", "Sulfonamide", "Sulfonate", "Sulfonate", "Fluoroalkoxy",
        "Sulfonate", "Fluoroalkyl", "Fluoroalkyl", "Fluoroalkoxy", "Fluoroalkyl",
        "Carboxylate", "Sulfonamide", "Sulfonate", "Sulfonamidoacetate",
        "Carboxylate", "Sulfonate", "Sulfonate", "Fluoroalkyl", "Sulfonamide",
        "Sulfonate", "Fluoroalkyl", "Sulfonate", "Fluoroalkoxy", "Sulfonamide",
        "Sulfonate", "Fluoroalkyl", "Sulfonamide", "Fluoroalkyl", "Fluoroalkoxy",
        "Sulfonamide", "Fluoroalkoxy", "Fluoroalkoxy", "Sulfonamide", "Fluoroalkyl",
        "Fluoroalkyl", "Fluoroalkoxy", "Fluoroalkoxy", "Fluoroalkyl", "Fluoroalkyl",
        "Fluoroalkoxy", "Fluoroalkyl", "Fluoroalkyl", "Fluoroalkyl", "Fluoroalkyl",
        "Fluoroalkyl"
      ),
      pfas_class = c(
        "PFSAs", "FOSAs", "PFSAs", "FTSs", "FTAs", "PFSAs", "PFCAs", "PFCAs",
        "GenX", "PFCAs", "PFOA", "FOSA", "PFOS", "FOSAA", "PFCAs", "PFSAs",
        "PFSAs", "PFCAs", "FOSAs", "PFSAs", "PFCAs", "PFSAs", "FTAs", "FOSAs",
        "PFSAs", "PFCAs", "FOSAs", "PFCAs", "FTAs", "FOSAs", "FTAs", "FTAs",
        "FOSAs", "PFCAs", "PFCAs", "FTAs", "FTAs", "PFCAs", "PFCAs", "FTAs",
        "PFCAs", "PFCAs", "FASAs", "PFCAs", "PFCAs"
      ),
      confidence_level = c(
        "High", "High", "High", "High", "Medium", "High", "High", "High",
        "High", "High", "High", "High", "High", "High", "Medium", "High",
        "High", "High", "High", "High", "Medium", "High", "Medium", "Medium",
        "High", "High", "Medium", "Medium", "Medium", "Medium", "Medium",
        "Medium", "Medium", "High", "Medium", "High", "Medium", "High",
        "Medium", "High", "Medium", "Medium", "Medium", "Medium", "Medium"
      ),
      frequency = c(
        3, 1, 5, 2, 1, 1, 6, 8, 1, 7, 1, 1, 1, 1, 1, 1, 2, 10, 1, 1, 3, 1,
        1, 1, 1, 1, 1, 3, 1, 1, 1, 1, 1, 1, 2, 1, 1, 1, 1, 1, 2, 1, 1, 1, 1
      ),
      stringsAsFactors = FALSE
    )
  })
  
  # 显示PFAS碎片预览
  output$pfas_fragments_preview <- renderText({
    fragments <- pfas_fragments()
    paste(
      sprintf("共 %d 个PFAS特征碎片", nrow(fragments)),
      "示例:",
      sprintf("1. m/z=%.5f, 化学式=%s, 类型=%s", 
              fragments$m_z[1], fragments$formula[1], fragments$pfas_class[1]),
      sprintf("2. m/z=%.5f, 化学式=%s, 类型=%s", 
              fragments$m_z[2], fragments$formula[2], fragments$pfas_class[2]),
      sprintf("3. m/z=%.5f, 化学式=%s, 类型=%s", 
              fragments$m_z[3], fragments$formula[3], fragments$pfas_class[3]),
      sprintf("4. m/z=%.5f, 化学式=%s, 类型=%s", 
              fragments$m_z[4], fragments$formula[4], fragments$pfas_class[4]),
      sprintf("5. m/z=%.5f, 化学式=%s, 类型=%s", 
              fragments$m_z[5], fragments$formula[5], fragments$pfas_class[5]),
      sep = "\n"
    )
  })
  
  # ==================== PFAS碎片检测函数 ====================
  detect_pfas_fragments <- function(ms2_spectrum, tolerance = 0.01) {
    if (is.null(ms2_spectrum) || nrow(ms2_spectrum) == 0) {
      return(data.frame())
    }
    
    pfas_list <- pfas_fragments()
    matched_fragments <- list()
    
    for (i in 1:nrow(ms2_spectrum)) {
      fragment_mz <- ms2_spectrum[i, 1]
      fragment_intensity <- ms2_spectrum[i, 2]
      
      # 在PFAS碎片列表中查找匹配
      mz_diff <- abs(pfas_list$m_z - fragment_mz)
      matches <- which(mz_diff <= tolerance)
      
      if (length(matches) > 0) {
        # 找到最佳匹配（最小的m/z差异）
        best_match_idx <- matches[which.min(mz_diff[matches])]
        best_match <- pfas_list[best_match_idx, ]
        
        matched_fragments[[length(matched_fragments) + 1]] <- data.frame(
          fragment_mz = fragment_mz,
          fragment_intensity = fragment_intensity,
          pfas_mz = best_match$m_z,
          formula = best_match$formula,
          compound_type = best_match$compound_type,
          pfas_class = best_match$pfas_class,
          confidence_level = best_match$confidence_level,
          mz_error = round(fragment_mz - best_match$m_z, 6),
          ppm_error = round((fragment_mz - best_match$m_z) / best_match$m_z * 1e6, 2),
          stringsAsFactors = FALSE
        )
      }
    }
    
    if (length(matched_fragments) > 0) {
      return(do.call(rbind, matched_fragments))
    } else {
      return(data.frame())
    }
  }
  
  # ==================== 文件上传自动处理 ====================
  # 显示文件信息
  output$file_info_ui <- renderUI({
    if (!is.null(input$compound_file) && !is.null(input$ms2_file)) {
      div(
        class = "file-info",
        tags$small(
          icon("check-circle", style = "color: #28a745; margin-right: 5px;"),
          tags$b("文件已上传:"),
          br(),
          paste("化合物文件:", tags$code(input$compound_file$name)),
          br(),
          paste("质谱文件:", tags$code(input$ms2_file$name))
        )
      )
    }
  })
  
  # 自动读取化合物数据
  observeEvent(input$compound_file, {
    req(input$compound_file)
    
    tryCatch({
      compounds <- read.csv(input$compound_file$datapath, stringsAsFactors = FALSE)
      
      # 验证必要的列
      required_cols <- c("name", "mz", "rt")
      missing_cols <- setdiff(required_cols, colnames(compounds))
      
      if (length(missing_cols) > 0) {
        showNotification(
          paste("CSV文件缺少必要的列:", paste(missing_cols, collapse = ", ")),
          type = "error",
          duration = 5
        )
        return()
      }
      
      # 检查RT单位，如果小于60，可能为分钟，转换为秒
      if(max(compounds$rt, na.rm = TRUE) < 60) {
        compounds$rt <- compounds$rt * 60
        showNotification("检测到保留时间单位为分钟，已自动转换为秒", 
                        type = "warning", duration = 3)
      }
      
      values$compounds <- compounds
      
      showNotification(
        paste("成功加载", nrow(compounds), "个化合物"),
        type = "message", duration = 3
      )
      
    }, error = function(e) {
      showNotification(paste("读取文件时出错:", e$message), 
                      type = "error", duration = 5)
    })
  })
  
  # ==================== 进度条UI ====================
  output$progress_ui <- renderUI({
    if (values$processing) {
      div(
        class = "progress-container",
        div(
          class = "progress-label",
          icon("cog", class = "fa-spin", style = "margin-right: 8px;"),
          tags$b(values$progress_text)
        ),
        div(
          class = "progress",
          style = "height: 20px; border-radius: 10px; overflow: hidden;",
          div(
            class = "progress-bar progress-bar-striped progress-bar-animated",
            role = "progressbar",
            style = paste0("width: ", values$progress_value, "%; background-color: #3498db;"),
            paste0(values$progress_value, "%")
          )
        )
      )
    }
  })
  
  # 更新进度函数
  update_progress <- function(value, text = NULL) {
    values$progress_value <- value
    if (!is.null(text)) {
      values$progress_text <- text
    }
  }
  
  # ==================== 数据库构建 ====================
  observeEvent(input$start_processing, {
    req(input$compound_file, input$ms2_file, values$compounds)
    
    # 重置进度
    update_progress(0, "开始构建数据库...")
    values$processing <- TRUE
    
    # 禁用开始按钮，防止重复点击
    shinyjs::disable("start_processing")
    
    # 步骤1: 读取质谱数据文件
    showNotification("步骤 1/5: 正在读取质谱数据文件...", 
                    type = "default", 
                    duration = NULL, 
                    id = "step_notification")
    update_progress(10, "读取质谱数据文件...")
    
    tryCatch({
      ms2_file_path <- input$ms2_file$datapath
      if (!file.exists(ms2_file_path)) {
        stop("MS2数据文件不存在")
      }
      
      msdata <- mzR::openMSfile(ms2_file_path)
      header_info <- mzR::header(msdata)
      
      # 步骤2: 提取MS2扫描信息
      removeNotification(id = "step_notification")
      showNotification("步骤 2/5: 正在提取MS2扫描信息...", 
                      type = "default", 
                      duration = NULL, 
                      id = "step_notification")
      update_progress(25, "提取MS2扫描信息...")
      
      ms2_scans <- header_info[header_info$msLevel == 2, ]
      
      if (nrow(ms2_scans) == 0) {
        mzR::close(msdata)
        stop("文件中没有MS2扫描数据")
      }
      
      # 提取MS2谱图
      ms2_spectra <- list()
      for (i in 1:nrow(ms2_scans)) {
        scan_num <- ms2_scans$seqNum[i]
        spectrum <- mzR::peaks(msdata, scan_num)
        
        if (nrow(spectrum) > 0) {
          spectrum <- spectrum[spectrum[, 2] >= input$min_intensity, , drop = FALSE]
          
          if (nrow(spectrum) >= 5) {
            top_n_peaks <- 50
            if (nrow(spectrum) > top_n_peaks) {
              spectrum <- spectrum[order(spectrum[, 2], decreasing = TRUE)[1:top_n_peaks], ]
              spectrum <- spectrum[order(spectrum[, 1]), ]
            }
            
            ms2_spectra[[i]] <- list(
              scan_num = scan_num,
              precursor_mz = ms2_scans$precursorMZ[i],
              precursor_intensity = ms2_scans$precursorIntensity[i],
              retention_time = ms2_scans$retentionTime[i],
              spectrum = spectrum
            )
          }
        }
      }
      
      ms2_spectra <- ms2_spectra[!sapply(ms2_spectra, is.null)]
      
      # 步骤3: 关联化合物与MS2谱图
      removeNotification(id = "step_notification")
      showNotification("步骤 3/5: 正在关联化合物与MS2谱图...", 
                      type = "default", 
                      duration = NULL, 
                      id = "step_notification")
      update_progress(45, "关联化合物与MS2谱图...")
      
      match_results <- data.frame(
        compound_id = integer(),
        compound_name = character(),
        compound_mz = numeric(),
        compound_rt = numeric(),
        scan_num = integer(),
        precursor_mz = numeric(),
        retention_time = numeric(),
        precursor_intensity = numeric(),
        match_quality = character(),
        n_peaks = integer(),
        stringsAsFactors = FALSE
      )
      
      compound_spectra <- list()
      total_compounds <- nrow(values$compounds)
      
      for (i in 1:total_compounds) {
        compound <- values$compounds[i, ]
        
        # 更新进度
        progress_value <- 45 + (i / total_compounds) * 25
        update_progress(progress_value, 
                       sprintf("处理化合物 %d/%d: %s", i, total_compounds, compound$name))
        
        mz_tolerance <- max(input$mz_tol, compound$mz * input$ppm / 1e6)
        candidate_spectra <- list()
        
        for (spec in ms2_spectra) {
          mz_diff <- abs(spec$precursor_mz - compound$mz)
          rt_diff <- abs(spec$retention_time - compound$rt)
          
          if (mz_diff <= mz_tolerance && rt_diff <= input$rt_tol) {
            mz_score <- 1 - (mz_diff / mz_tolerance)
            rt_score <- 1 - (rt_diff / input$rt_tol)
            intensity_score <- log10(spec$precursor_intensity + 1) / 10
            
            total_score <- mz_score * 0.5 + rt_score * 0.3 + intensity_score * 0.2
            
            candidate_spectra[[length(candidate_spectra) + 1]] <- list(
              spec_data = spec,
              mz_diff = mz_diff,
              rt_diff = rt_diff,
              score = total_score
            )
          }
        }
        
        if (length(candidate_spectra) > 0) {
          scores <- sapply(candidate_spectra, function(x) x$score)
          best_idx <- which.max(scores)
          best_match <- candidate_spectra[[best_idx]]
          spec <- best_match$spec_data
          
          if (best_match$score > 0.8) {
            quality <- "excellent"
          } else if (best_match$score > 0.6) {
            quality <- "good"
          } else if (best_match$score > 0.4) {
            quality <- "fair"
          } else {
            quality <- "poor"
          }
          
          match_results <- rbind(match_results, data.frame(
            compound_id = i,
            compound_name = compound$name,
            compound_mz = compound$mz,
            compound_rt = compound$rt,
            scan_num = spec$scan_num,
            precursor_mz = spec$precursor_mz,
            retention_time = spec$retention_time,
            precursor_intensity = spec$precursor_intensity,
            match_quality = quality,
            n_peaks = nrow(spec$spectrum),
            stringsAsFactors = FALSE
          ))
          
          # 提取EIC数据
          eic_data <- extract_eic_for_compound(
            msdata = msdata,
            compound_mz = compound$mz,
            compound_rt = spec$retention_time,
            mz_tolerance = input$eic_mz_tol,
            rt_window = input$eic_rt_window
          )
          
          # 检测PFAS碎片
          pfas_matches <- data.frame()
          if (input$enable_pfas_detection && !is.null(spec$spectrum)) {
            pfas_matches <- detect_pfas_fragments(
              spec$spectrum, 
              tolerance = input$pfas_mz_tol
            )
          }
          
          compound_spectra[[i]] <- list(
            compound_name = compound$name,
            compound_mz = compound$mz,
            compound_rt = compound$rt,
            spectrum = spec$spectrum,
            scan_num = spec$scan_num,
            precursor_mz = spec$precursor_mz,
            precursor_intensity = spec$precursor_intensity,
            retention_time = spec$retention_time,
            eic_data = eic_data,
            match_score = best_match$score,
            match_quality = quality,
            pfas_matches = pfas_matches
          )
        } else {
          match_results <- rbind(match_results, data.frame(
            compound_id = i,
            compound_name = compound$name,
            compound_mz = compound$mz,
            compound_rt = compound$rt,
            scan_num = NA,
            precursor_mz = NA,
            retention_time = NA,
            precursor_intensity = NA,
            match_quality = "no_match",
            n_peaks = 0,
            stringsAsFactors = FALSE
          ))
          
          compound_spectra[[i]] <- list(
            compound_name = compound$name,
            compound_mz = compound$mz,
            compound_rt = compound$rt,
            spectrum = NULL,
            eic_data = NULL,
            match_quality = "no_match",
            pfas_matches = data.frame()
          )
        }
      }
      
      # 步骤4: 构建数据库对象
      removeNotification(id = "step_notification")
      showNotification("步骤 4/5: 正在构建数据库...", 
                      type = "default", 
                      duration = NULL, 
                      id = "step_notification")
      update_progress(85, "构建数据库...")
      
      database <- list(
        metadata = list(
          creation_date = Sys.time(),
          input_files = list(
            compound_csv = input$compound_file$name,
            ms2_file = input$ms2_file$name
          ),
          parameters = list(
            mz_tol = input$mz_tol,
            ppm = input$ppm,
            rt_tol = input$rt_tol,
            min_intensity = input$min_intensity,
            eic_mz_tol = input$eic_mz_tol,
            eic_rt_window = input$eic_rt_window,
            enable_pfas_detection = input$enable_pfas_detection,
            pfas_mz_tol = if(input$enable_pfas_detection) input$pfas_mz_tol else NA
          ),
          summary = list(
            n_compounds = total_compounds,
            n_matched = sum(match_results$match_quality != "no_match"),
            match_quality = table(match_results$match_quality)
          )
        ),
        compounds = values$compounds,
        match_results = match_results,
        spectra = compound_spectra
      )
      
      # 步骤5: 计算PFAS统计
      if (input$enable_pfas_detection) {
        removeNotification(id = "step_notification")
        showNotification("步骤 5/5: 正在计算PFAS统计...", 
                        type = "default", 
                        duration = NULL, 
                        id = "step_notification")
        update_progress(95, "计算PFAS统计...")
        
        # 计算PFAS检测统计
        pfas_detected <- sapply(compound_spectra, function(x) {
          if (!is.null(x$pfas_matches) && nrow(x$pfas_matches) > 0) {
            return(TRUE)
          }
          return(FALSE)
        })
        
        database$metadata$pfas_summary <- list(
          n_compounds_with_pfas = sum(pfas_detected),
          pfas_detection_rate = round(sum(pfas_detected) / total_compounds * 100, 2),
          total_pfas_fragments = sum(sapply(compound_spectra, function(x) {
            if (!is.null(x$pfas_matches)) nrow(x$pfas_matches) else 0
          }))
        )
      }
      
      values$database <- database
      
      # 关闭质谱文件
      mzR::close(msdata)
      
      update_progress(100, "数据库构建完成！")
      
      # 延迟后完成处理
      shinyjs::delay(1000, {
        removeNotification(id = "step_notification")
        
        match_count <- sum(match_results$match_quality != "no_match")
        msg <- paste("✅ 数据库构建完成！共匹配", match_count, "个化合物")
        
        if (input$enable_pfas_detection && exists("pfas_detected")) {
          pfas_count <- sum(pfas_detected)
          msg <- paste(msg, sprintf("，其中 %d 个化合物检测到PFAS特征碎片", pfas_count))
        }
        
        showNotification(msg, type = "message", duration = 5)
        
        values$processing <- FALSE
        shinyjs::enable("start_processing")
      })
      
    }, error = function(e) {
      update_progress(0, paste("错误:", e$message))
      removeNotification(id = "step_notification")
      showNotification(
        paste("❌ 构建数据库时出错:", e$message),
        type = "error", duration = 10
      )
      values$processing = FALSE
      shinyjs::enable("start_processing")
    })
  })
  
  # ==================== 数据库表格显示 ====================
  output$database_table <- renderDT({
    req(values$database)
    
    display_data <- values$database$match_results
    
    # 添加PFAS匹配信息列
    if (input$enable_pfas_detection) {
      display_data$pfas_info <- sapply(1:nrow(display_data), function(i) {
        if (display_data$match_quality[i] != "no_match") {
          spec <- values$database$spectra[[display_data$compound_id[i]]]
          if (!is.null(spec$pfas_matches) && nrow(spec$pfas_matches) > 0) {
            matches <- spec$pfas_matches
            
            # 为每个匹配创建HTML标签
            html_tags <- sapply(1:min(2, nrow(matches)), function(j) {
              confidence_class <- if (matches$confidence_level[j] == "High") "pfas-high" else "pfas-medium"
              sprintf('<div class="pfas-fragment %s" title="m/z误差: %.5f Da, ppm误差: %.1f">
                      %s (%.4f)</div>',
                      confidence_class,
                      matches$mz_error[j],
                      matches$ppm_error[j],
                      matches$formula[j],
                      matches$fragment_mz[j])
            })
            
            # 如果有更多匹配，添加省略号
            if (nrow(matches) > 2) {
              html_tags <- c(html_tags, 
                            sprintf('<div class="pfas-fragment">+%d more</div>', 
                                    nrow(matches) - 2))
            }
            
            paste(html_tags, collapse = "")
          } else {
            '<span style="color: #6c757d; font-style: italic;">无PFAS碎片</span>'
          }
        } else {
          ""
        }
      })
    }
    
    # 创建操作按钮列
    display_data$action <- sapply(1:nrow(display_data), function(i) {
      if (display_data$match_quality[i] != "no_match") {
        as.character(
          actionButton(
            paste0("view_", i),
            label = if (input$enable_pfas_detection) "查看谱图(PFAS)" else "查看谱图",
            icon = icon("chart-line"),
            class = "btn-sm btn-primary",
            onclick = sprintf("Shiny.setInputValue('view_row', %d);", i)
          )
        )
      } else {
        "无匹配"
      }
    })
    
    # 设置列名
    col_names <- c(
      "ID", "化合物名称", "化合物m/z", "化合物RT", 
      "扫描号", "前体m/z", "RT", "前体强度", 
      "匹配质量", "峰数"
    )
    
    if (input$enable_pfas_detection) {
      col_names <- c(col_names, "PFAS碎片匹配", "操作")
    } else {
      col_names <- c(col_names, "操作")
    }
    
    datatable(
      display_data,
      extensions = c('Buttons', 'Scroller'),
      options = list(
        pageLength = 15,
        scrollX = TRUE,
        scrollY = "450px",
        scroller = TRUE,
        searching = TRUE,
        dom = 'Bfrtip',
        buttons = c('pageLength', 'colvis'),
        columnDefs = list(
          list(
            targets = if (input$enable_pfas_detection) 10 else 9,  # 匹配质量列
            render = JS(
              "function(data, type, row, meta) {
                if (data === 'excellent') {
                  return '<span class=\"badge\" style=\"background-color: #28a745; color: white; padding: 4px 8px; border-radius: 12px;\">' + data + '</span>';
                } else if (data === 'good') {
                  return '<span class=\"badge\" style=\"background-color: #007bff; color: white; padding: 4px 8px; border-radius: 12px;\">' + data + '</span>';
                } else if (data === 'fair') {
                  return '<span class=\"badge\" style=\"background-color: #ffc107; color: white; padding: 4px 8px; border-radius: 12px;\">' + data + '</span>';
                } else if (data === 'poor') {
                  return '<span class=\"badge\" style=\"background-color: #dc3545; color: white; padding: 4px 8px; border-radius: 12px;\">' + data + '</span>';
                } else {
                  return '<span class=\"badge\" style=\"background-color: #6c757d; color: white; padding: 4px 8px; border-radius: 12px;\">' + data + '</span>';
                }
              }"
            )
          ),
          list(
            targets = ncol(display_data) - 1,  # 操作列
            orderable = FALSE,
            searchable = FALSE
          )
        )
      ),
      class = 'cell-border stripe hover compact',
      selection = 'none',
      rownames = FALSE,
      escape = FALSE,
      colnames = col_names
    ) %>% 
      formatRound(columns = c('compound_mz', 'precursor_mz'), digits = 4) %>%
      formatRound(columns = c('compound_rt', 'retention_time'), digits = 2)
  })
  
  # ==================== PFAS统计图表 ====================
  output$pfas_detection_plot <- renderPlotly({
    req(values$database, input$enable_pfas_detection)
    
    # 统计每个化合物的PFAS碎片数量
    pfas_counts <- sapply(values$database$spectra, function(x) {
      if (!is.null(x$pfas_matches)) {
        nrow(x$pfas_matches)
      } else {
        0
      }
    })
    
    # 创建统计数据
    detection_stats <- data.frame(
      category = c("有PFAS碎片", "无PFAS碎片"),
      count = c(sum(pfas_counts > 0), sum(pfas_counts == 0))
    )
    
    plot_ly(
      detection_stats,
      labels = ~category,
      values = ~count,
      type = 'pie',
      marker = list(
        colors = c('#ff6b6b', '#4ecdc4'),
        line = list(color = '#FFFFFF', width = 2)
      ),
      textposition = 'inside',
      textinfo = 'label+percent',
      hovertemplate = paste(
        "<b>%{label}</b><br>",
        "数量: %{value}<br>",
        "百分比: %{percent}<br>",
        "<extra></extra>"
      )
    ) %>%
      layout(
        title = list(
          text = paste("PFAS检测统计 (总计:", sum(pfas_counts > 0), "/", length(pfas_counts), ")"),
          font = list(size = 14)
        ),
        showlegend = TRUE,
        legend = list(
          orientation = "v",
          yanchor = "middle",
          y = 0.5,
          xanchor = "right",
          x = 1.2
        ),
        margin = list(l = 20, r = 150, t = 40, b = 20)
      )
  })
  
  output$pfas_class_plot <- renderPlotly({
    req(values$database, input$enable_pfas_detection)
    
    # 收集所有PFAS碎片的类别
    pfas_classes <- c()
    
    for (spec in values$database$spectra) {
      if (!is.null(spec$pfas_matches) && nrow(spec$pfas_matches) > 0) {
        pfas_classes <- c(pfas_classes, spec$pfas_matches$pfas_class)
      }
    }
    
    if (length(pfas_classes) == 0) {
      # 如果没有检测到PFAS碎片，显示空图表
      plot_ly() %>%
        add_annotations(
          text = "未检测到PFAS碎片",
          xref = "paper",
          yref = "paper",
          x = 0.5,
          y = 0.5,
          showarrow = FALSE,
          font = list(size = 16, color = "#7f8c8d")
        ) %>%
        layout(
          title = list(text = "PFAS类别分布", font = list(size = 14)),
          xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
          yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
          margin = list(l = 60, r = 30, t = 40, b = 60),
          plot_bgcolor = 'rgba(240, 240, 240, 0.5)'
        )
    } else {
      # 统计每个类别的数量
      class_counts <- as.data.frame(table(pfas_classes))
      colnames(class_counts) <- c("Class", "Count")
      
      # 按数量排序
      class_counts <- class_counts[order(-class_counts$Count), ]
      
      plot_ly(
        class_counts,
        x = ~Count,
        y = ~reorder(Class, Count),
        type = 'bar',
        orientation = 'h',
        marker = list(
          color = 'rgba(52, 152, 219, 0.7)',
          line = list(color = 'rgba(41, 128, 185, 1.0)', width = 1)
        ),
        text = ~Count,
        textposition = 'auto',
        hovertemplate = paste(
          "<b>%{y}</b><br>",
          "出现次数: %{x}<br>",
          "<extra></extra>"
        )
      ) %>%
        layout(
          title = list(text = "PFAS类别分布", font = list(size = 14)),
          xaxis = list(title = "出现次数", tickfont = list(size = 11)),
          yaxis = list(title = "", tickfont = list(size = 11)),
          margin = list(l = 150, r = 30, t = 40, b = 60),
          plot_bgcolor = 'rgba(240, 240, 240, 0.5)'
        )
    }
  })
  
  # ==================== 弹窗可视化 ====================
  # 监听查看按钮点击
  observeEvent(input$view_row, {
    req(values$database, input$view_row)
    
    row_index <- input$view_row
    if (row_index <= nrow(values$database$match_results)) {
      selected_compound <- values$database$match_results[row_index, ]
      spec <- values$database$spectra[[selected_compound$compound_id]]
      
      if (!is.null(spec)) {
        values$selected_spec <- spec
        
        # 更新弹窗标题
        output$modal_title <- renderText({
          if (input$enable_pfas_detection && !is.null(spec$pfas_matches) && nrow(spec$pfas_matches) > 0) {
            paste(selected_compound$compound_name, "- MS2谱图 (检测到", nrow(spec$pfas_matches), "个PFAS碎片)")
          } else {
            paste(selected_compound$compound_name, "- MS2谱图")
          }
        })
        
        # 更新EIC图
        output$modal_eic_plot <- renderPlotly({
          if (!is.null(spec$eic_data) && nrow(spec$eic_data) > 0) {
            eic_data <- spec$eic_data
            eic_ms1 <- eic_data[eic_data$msLevel == 1, ]
            eic_ms2 <- eic_data[eic_data$msLevel == 2, ]
            
            # 确保EIC数据正确排序
            eic_ms1 <- eic_ms1[order(eic_ms1$retentionTime), ]
            eic_ms2 <- eic_ms2[order(eic_ms2$retentionTime), ]
            
            # 计算合适的y轴范围
            y_max <- if (nrow(eic_ms1) > 0) {
              max(eic_ms1$EIC, na.rm = TRUE)
            } else {
              0
            }
            
            # 如果有MS2点，考虑它们的高度
            if (nrow(eic_ms2) > 0) {
              y_max <- max(y_max, max(eic_ms2$EIC, na.rm = TRUE))
            }
            
            # 添加一些余量
            if (y_max > 0) {
              y_max <- y_max * 1.1
            } else {
              y_max <- 1
            }
            
            plot_ly() %>%
              add_trace(
                data = eic_ms1,
                x = ~retentionTime,
                y = ~EIC,
                type = 'scatter',
                mode = 'lines',
                name = 'MS1 EIC',
                line = list(color = 'rgb(65, 105, 225)', width = 2.5),
                fill = 'tozeroy',
                fillcolor = 'rgba(65, 105, 225, 0.1)',
                hovertemplate = paste(
                  "<b>MS1扫描</b><br>",
                  "RT: %{x:.2f} s<br>",
                  "强度: %{y:.2e}",
                  "<extra></extra>"
                )
              ) %>%
              add_trace(
                data = eic_ms2,
                x = ~retentionTime,
                y = ~EIC,
                type = 'scatter',
                mode = 'markers',
                name = 'MS2触发点',
                marker = list(
                  color = 'rgb(220, 53, 69)',
                  size = 12,
                  symbol = 'triangle-up',
                  line = list(color = 'white', width = 1)
                ),
                hovertemplate = paste(
                  "<b>MS2扫描 #%{text}</b><br>",
                  "RT: %{x:.2f} s<br>",
                  "强度: %{y:.2e}",
                  "<extra></extra>"
                ),
                text = ~scanNum
              ) %>%
              layout(
                title = list(text = "提取离子色谱图 (EIC)", font = list(size = 16)),
                xaxis = list(
                  title = '保留时间 (秒)',
                  tickfont = list(size = 12),
                  gridcolor = 'rgba(200, 200, 200, 0.3)',
                  zeroline = TRUE,
                  zerolinecolor = 'rgba(150, 150, 150, 0.5)',
                  zerolinewidth = 1
                ),
                yaxis = list(
                  title = 'EIC强度',
                  tickfont = list(size = 12),
                  gridcolor = 'rgba(200, 200, 200, 0.3)',
                  zeroline = TRUE,
                  zerolinecolor = 'rgba(150, 150, 150, 0.5)',
                  zerolinewidth = 1,
                  range = c(0, y_max)
                ),
                hovermode = 'closest',
                margin = list(l = 60, r = 40, t = 60, b = 60),
                plot_bgcolor = 'rgba(248, 249, 250, 1)',
                showlegend = TRUE,
                legend = list(
                  orientation = "h",
                  yanchor = "bottom",
                  y = 1.02,
                  xanchor = "right",
                  x = 1
                )
              )
          }
        })
        
        # 更新MS2谱图
        output$modal_ms2_plot <- renderPlotly({
          if (!is.null(values$selected_spec) && !is.null(values$selected_spec$spectrum)) {
            spec <- values$selected_spec
            
            spectrum_df <- data.frame(
              mz = spec$spectrum[, 1],
              intensity = spec$spectrum[, 2],
              relative_intensity = 100 * spec$spectrum[, 2] / max(spec$spectrum[, 2])
            )
            
            # 识别PFAS碎片
            pfas_peaks <- NULL
            pfas_info <- NULL
            if (input$enable_pfas_detection && !is.null(spec$pfas_matches) && nrow(spec$pfas_matches) > 0) {
              # 确保PFAS碎片在谱图中
              pfas_peaks <- spectrum_df[spectrum_df$mz %in% spec$pfas_matches$fragment_mz, ]
              
              # 为PFAS碎片添加信息标签
              if (nrow(pfas_peaks) > 0) {
                pfas_info <- lapply(1:nrow(pfas_peaks), function(i) {
                  peak_mz <- pfas_peaks$mz[i]
                  match <- spec$pfas_matches[spec$pfas_matches$fragment_mz == peak_mz, ]
                  if (nrow(match) > 0) {
                    sprintf("化学式: %s<br>类别: %s<br>m/z误差: %.5f Da<br>ppm误差: %.1f", 
                            match$formula[1], 
                            match$pfas_class[1], 
                            match$mz_error[1],
                            match$ppm_error[1])
                  } else {
                    ""
                  }
                })
              }
            }
            
            # 显示前30个峰
            top_n <- min(30, nrow(spectrum_df))
            if (nrow(spectrum_df) > top_n) {
              spectrum_df <- spectrum_df[order(spectrum_df$intensity, decreasing = TRUE)[1:top_n], ]
              spectrum_df <- spectrum_df[order(spectrum_df$mz), ]
            }
            
            # 为前10个峰添加标签
            top_peaks <- spectrum_df[order(spectrum_df$intensity, decreasing = TRUE)[1:min(10, nrow(spectrum_df))], ]
            
            # 创建基础图形
            p <- plot_ly()
            
            # 添加所有碎片离子
            p <- p %>% add_segments(
              data = spectrum_df,
              x = ~mz,
              xend = ~mz,
              y = 0,
              yend = ~relative_intensity,
              name = '碎片离子',
              line = list(color = 'rgb(30, 144, 255)', width = 2),
              hovertemplate = paste(
                "<b>碎片离子</b><br>",
                "m/z: %{x:.4f}<br>",
                "相对强度: %{y:.1f}%<br>",
                "绝对强度: %{text:.2e}",
                "<extra></extra>"
              ),
              text = ~intensity
            )
            
            # 如果有PFAS碎片，用红色标记
            if (!is.null(pfas_peaks) && nrow(pfas_peaks) > 0 && !is.null(pfas_info)) {
              p <- p %>% add_segments(
                data = pfas_peaks,
                x = ~mz,
                xend = ~mz,
                y = 0,
                yend = ~relative_intensity,
                name = 'PFAS特征碎片',
                line = list(color = 'rgb(220, 53, 69)', width = 3),
                hovertemplate = paste(
                  "<b>PFAS特征碎片</b><br>",
                  "m/z: %{x:.4f}<br>",
                  "相对强度: %{y:.1f}%<br>",
                  "%{customdata}",
                  "<extra></extra>"
                ),
                customdata = pfas_info
              )
            }
            
            # 添加标注
            if (nrow(top_peaks) > 0) {
              for (i in 1:nrow(top_peaks)) {
                p <- p %>% add_annotations(
                  x = top_peaks$mz[i],
                  y = top_peaks$relative_intensity[i],
                  text = sprintf('%.4f', top_peaks$mz[i]),
                  showarrow = TRUE,
                  arrowhead = 1,
                  arrowsize = 1,
                  arrowwidth = 1.5,
                  arrowcolor = 'rgb(178, 34, 34)',
                  ax = 0,
                  ay = -40,
                  font = list(size = 11, color = 'rgb(178, 34, 34)', family = 'Arial')
                )
              }
            }
            
            # 设置布局
            p <- p %>% layout(
              title = list(
                text = "MS2质谱图",
                font = list(size = 16)
              ),
              xaxis = list(
                title = 'm/z',
                tickfont = list(size = 12),
                gridcolor = 'rgba(200, 200, 200, 0.3)',
                zeroline = TRUE,
                zerolinecolor = 'rgba(150, 150, 150, 0.5)',
                zerolinewidth = 1
              ),
              yaxis = list(
                title = '相对强度 (%)',
                tickfont = list(size = 12),
                gridcolor = 'rgba(200, 200, 200, 0.3)',
                zeroline = TRUE,
                zerolinecolor = 'rgba(150, 150, 150, 0.5)',
                zerolinewidth = 1,
                range = c(0, 105)
              ),
              hovermode = 'closest',
              showlegend = TRUE,
              legend = list(
                orientation = "h",
                yanchor = "bottom",
                y = 1.02,
                xanchor = "right",
                x = 1
              ),
              margin = list(l = 60, r = 40, t = 60, b = 60),
              plot_bgcolor = 'rgba(248, 249, 250, 1)'
            )
            
            return(p)
          }
        })
        
        # 使用JavaScript打开弹窗
        shinyjs::runjs("showVisualizationModal();")
      }
    }
  })
  
  # ==================== 下载功能 ====================
  output$download_csv <- downloadHandler(
    filename = function() {
      paste0("ms2_database_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
    },
    content = function(file) {
      req(values$database)
      
      csv_data <- values$database$match_results
      
      # 添加MS2峰信息
      mz_columns <- sapply(values$database$spectra, function(x) {
        if (!is.null(x$spectrum)) {
          paste(sprintf("%.4f", x$spectrum[, 1]), collapse = " ")
        } else {
          ""
        }
      })
      
      intensity_columns <- sapply(values$database$spectra, function(x) {
        if (!is.null(x$spectrum)) {
          paste(sprintf("%.0f", x$spectrum[, 2]), collapse = " ")
        } else {
          ""
        }
      })
      
      csv_data$ms2_mz <- mz_columns
      csv_data$ms2_intensity <- intensity_columns
      
      # 添加PFAS匹配信息
      if (input$enable_pfas_detection) {
        pfas_info <- sapply(values$database$spectra, function(x) {
          if (!is.null(x$pfas_matches) && nrow(x$pfas_matches) > 0) {
            matches <- x$pfas_matches
            paste(
              sapply(1:nrow(matches), function(i) {
                sprintf("%s(m/z=%.4f, Δm/z=%.5f, ppm=%.1f, type=%s)", 
                       matches$formula[i],
                       matches$fragment_mz[i],
                       matches$mz_error[i],
                       matches$ppm_error[i],
                       matches$pfas_class[i])
              }),
              collapse = "; "
            )
          } else {
            ""
          }
        })
        csv_data$pfas_matches <- pfas_info
      }
      
      write.csv(csv_data, file, row.names = FALSE)
    }
  )
  
  output$download_rds <- downloadHandler(
    filename = function() {
      paste0("ms2_database_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".rds")
    },
    content = function(file) {
      req(values$database)
      saveRDS(values$database, file)
    }
  )
  
  output$download_msp <- downloadHandler(
    filename = function() {
      paste0("ms2_database_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".msp")
    },
    content = function(file) {
      req(values$database)
      
      msp_con <- file(file, "w")
      
      for (spec in values$database$spectra) {
        if (!is.null(spec$spectrum)) {
          writeLines(sprintf("Name: %s", spec$compound_name), msp_con)
          writeLines(sprintf("MW: %.4f", spec$compound_mz), msp_con)
          writeLines(sprintf("RT: %.2f", spec$compound_rt), msp_con)
          writeLines(sprintf("PrecursorMZ: %.4f", spec$precursor_mz), msp_con)
          writeLines(sprintf("PrecursorIntensity: %.0f", spec$precursor_intensity), msp_con)
          writeLines(sprintf("ScanNum: %d", spec$scan_num), msp_con)
          
          # 添加PFAS信息
          if (input$enable_pfas_detection && !is.null(spec$pfas_matches) && nrow(spec$pfas_matches) > 0) {
            writeLines(sprintf("PFAS_Fragments: %d", nrow(spec$pfas_matches)), msp_con)
            for (i in 1:nrow(spec$pfas_matches)) {
              writeLines(sprintf("PFAS%d: %s (%.4f)", i, 
                                spec$pfas_matches$formula[i],
                                spec$pfas_matches$fragment_mz[i]), msp_con)
            }
          }
          
          writeLines(sprintf("Num Peaks: %d", nrow(spec$spectrum)), msp_con)
          
          for (j in 1:nrow(spec$spectrum)) {
            writeLines(sprintf("%.4f\t%.0f", spec$spectrum[j, 1], spec$spectrum[j, 2]), msp_con)
          }
          
          writeLines("", msp_con)
        }
      }
      
      close(msp_con)
    }
  )
  
  output$download_pfas_report <- downloadHandler(
    filename = function() {
      paste0("pfas_detection_report_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
    },
    content = function(file) {
      req(values$database)
      
      # 收集所有PFAS匹配结果
      all_pfas_matches <- data.frame()
      
      for (i in 1:length(values$database$spectra)) {
        spec <- values$database$spectra[[i]]
        if (!is.null(spec$pfas_matches) && nrow(spec$pfas_matches) > 0) {
          compound_info <- values$database$match_results[i, ]
          matches <- spec$pfas_matches
          
          report_df <- data.frame(
            compound_name = compound_info$compound_name,
            compound_mz = compound_info$compound_mz,
            compound_rt = compound_info$compound_rt,
            precursor_mz = spec$precursor_mz,
            precursor_intensity = spec$precursor_intensity,
            retention_time = spec$retention_time,
            fragment_mz = matches$fragment_mz,
            fragment_intensity = matches$fragment_intensity,
            fragment_relative_intensity = round(100 * matches$fragment_intensity / max(spec$spectrum[, 2]), 1),
            pfas_formula = matches$formula,
            pfas_mz = matches$pfas_mz,
            pfas_class = matches$pfas_class,
            compound_type = matches$compound_type,
            confidence_level = matches$confidence_level,
            mz_error = matches$mz_error,
            ppm_error = matches$ppm_error,
            stringsAsFactors = FALSE
          )
          
          all_pfas_matches <- rbind(all_pfas_matches, report_df)
        }
      }
      
      if (nrow(all_pfas_matches) > 0) {
        write.csv(all_pfas_matches, file, row.names = FALSE)
      } else {
        write.csv(data.frame(Message = "未检测到PFAS特征碎片"), file, row.names = FALSE)
      }
    }
  )
  
  output$download_combined <- downloadHandler(
    filename = function() {
      req(values$selected_spec)
      paste0(gsub("[^[:alnum:]]", "_", values$selected_spec$compound_name), 
             "_spectra_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
    },
    content = function(file) {
      req(values$selected_spec)
      
      spec <- values$selected_spec
      
      # 创建数据框
      result_data <- data.frame(
        Compound = spec$compound_name,
        Precursor_mz = spec$precursor_mz,
        Precursor_intensity = spec$precursor_intensity,
        Retention_time = spec$retention_time,
        Scan_number = spec$scan_num,
        Match_quality = spec$match_quality,
        Match_score = spec$match_score
      )
      
      # 如果有PFAS匹配，添加信息
      if (input$enable_pfas_detection && !is.null(spec$pfas_matches) && nrow(spec$pfas_matches) > 0) {
        pfas_data <- data.frame(
          PFAS_Formula = spec$pfas_matches$formula,
          PFAS_Class = spec$pfas_matches$pfas_class,
          Fragment_mz = spec$pfas_matches$fragment_mz,
          Fragment_intensity = spec$pfas_matches$fragment_intensity,
          mz_error = spec$pfas_matches$mz_error,
          ppm_error = spec$pfas_matches$ppm_error,
          Confidence = spec$pfas_matches$confidence_level
        )
        
        # 合并数据
        all_data <- rbind(
          data.frame(Parameter = names(result_data), Value = t(result_data)[1,]),
          data.frame(Parameter = "---", Value = "---"),
          data.frame(Parameter = "PFAS_Fragments", Value = "---"),
          pfas_data
        )
      } else if (!is.null(spec$spectrum)) {
        spectrum_data <- data.frame(
          mz = spec$spectrum[, 1],
          intensity = spec$spectrum[, 2],
          relative_intensity = 100 * spec$spectrum[, 2] / max(spec$spectrum[, 2])
        )
        
        all_data <- rbind(
          data.frame(Parameter = names(result_data), Value = t(result_data)[1,]),
          data.frame(Parameter = "---", Value = "---"),
          data.frame(Parameter = "Spectrum_Data", Value = "---"),
          spectrum_data
        )
      } else {
        all_data <- data.frame(Parameter = names(result_data), Value = t(result_data)[1,])
      }
      
      write.csv(all_data, file, row.names = FALSE)
    }
  )
}

# ==================== 运行Shiny应用 ====================

shinyApp(ui = ui, server = server)
