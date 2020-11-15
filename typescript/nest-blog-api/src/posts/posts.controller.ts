import {
  Controller,
  Get,
  Post,
  Body,
  Param,
  Put,
  Delete,
} from '@nestjs/common';
import { ApiTags, ApiOperation, ApiProperty } from '@nestjs/swagger';
import { IsNotEmpty } from 'class-validator';
import { InjectModel } from 'nestjs-typegoose';
import { Post as PostSchema } from './post.model';
import { ModelType } from '@typegoose/typegoose/lib/types';

class CreatePostDto {
  @ApiProperty({ description: 'title', example: 'Title' })
  @IsNotEmpty({ message: 'please enter the title' })
  title: string;
  @ApiProperty({ description: 'content', example: 'Content' })
  content: string;
}

@Controller('posts')
@ApiTags('Posts')
export class PostsController {
  constructor(
    @InjectModel(PostSchema) private readonly postModel: ModelType<PostSchema>,
  ) {}

  @Get()
  @ApiOperation({ summary: 'Show list of posts' })
  async index() {
    return await this.postModel.find();
  }

  @Post()
  @ApiOperation({ summary: 'New post' })
  async create(@Body() createPostDto: CreatePostDto) {
    await this.postModel.create(createPostDto);
    return {
      success: true,
    };
  }

  @Get(':id')
  @ApiOperation({ summary: 'About the post' })
  async detail(@Param('id') id: string) {
    return await this.postModel.findById(id);
  }

  @Put(':id')
  @ApiOperation({ summary: 'Edit the post' })
  async update(@Param('id') id: string, @Body() updatePostDto: CreatePostDto) {
    return await this.postModel.findByIdAndUpdate(id, updatePostDto);
  }

  @Delete(':id')
  @ApiOperation({ summary: 'Delete the post' })
  async remove(@Param('id') id: string) {
    return await this.postModel.findByIdAndRemove(id);
  }
}
