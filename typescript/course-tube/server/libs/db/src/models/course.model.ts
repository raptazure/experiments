import { prop, modelOptions, arrayProp, Ref } from '@typegoose/typegoose';
import { ApiProperty } from '@nestjs/swagger';
import { Episode } from './episode.model';

@modelOptions({
  schemaOptions: {
    timestamps: true,
  },
})
export class Course {
  @ApiProperty({ description: 'Course Name' })
  @prop()
  name: string;

  @ApiProperty({ description: 'Course Cover' })
  @prop()
  cover: string;

  @arrayProp({ itemsRef: 'Episode' })
  episodes: Ref<Episode>[];
}
